; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(defpackage :cl-fuse-meta-fs
  (:use 
   :common-lisp
   :cl-fuse
   :iterate
   :bordeaux-threads
   )
  (:export #:run-lisp-meta-fs
           #:mk-file
           #:mk-dir
           #:mk-symlink
           #:mk-symlinker
           #:mk-generator
           #:mk-pair-generator
           #:mk-creator
           #:mk-dir-remover
           #:fmt
           #:id
           #:cached-expr
           #:*object-cache-duration*
	   #:*meta-fs-name-encoding*
           )
  )

(in-package :cl-fuse-meta-fs)

#+nil 
(
  (:type :file name (contents) (writer data) (remover))
  (:type :dir name (contents))
  (:type :symlink name (contents) (remover))
  (:type :generate (parser x) (predicate x) (lister) (contents x))
  (:type :creator (create-file name) (create-dir name))
  )

(defparameter *meta-fs-name-encoding* :full-range)
(defun decode-name (n) 
  (cond
   ((stringp n) n)
   ((arrayp n) (cl-fuse::octets-to-string n *meta-fs-name-encoding*))
   ((listp n) (format nil "~{/~a~}" (mapcar 'decode-name n)))
   (t (format nil "error-decoding-name:~s~%" n))
   ))

(defmacro mk-file (name contents &optional (writer nil) (remover nil) (size nil) (executable nil))
  ``(:type :file :name ,(decode-name ,name) :contents ,(lambda () ,contents) 
           :writer ,,(cond 
                     ((null writer) nil)
                     ((symbolp writer) `',writer)
                     (t `(lambda (,(car writer)) 
                                 (declare (ignorable ,(car writer)))
                                 ,@(cdr writer)))
                     )
           :remover ,,(when remover `(lambda () ,remover))
	   :size ,,(if size `(lambda () ,size))
	   :executable ,,executable
           ))

(defmacro mk-dir (name kind &rest contents)
  ``(:type :dir :name ,(decode-name ,name) :contents 
          ,(lambda () 
                  ,(case kind
                       (:just `(list ,@contents))
                       (:eval `(progn ,@contents))
                    ))))

(defmacro mk-symlink (name contents &optional (remover nil))
  ``(:type :symlink :name ,(decode-name ,name) 
           :contents ,(lambda () (decode-name ,contents))
           :remover ,,(when remover `(lambda () ,remover))))

(defmacro mk-generator (var parser predicate lister contents)
  ``(:type :generate 
          :parser ,,(if (symbolp parser) `',parser `(lambda (,var) ,parser))
          :predicate ,,(if (symbolp predicate) `',predicate `(lambda (,var) ,predicate))
          :lister ,(lambda () ,lister)
          :contents ,(lambda (,var) ,contents)))

(defvar *cache-storage* (make-hash-table :test 'equal))
(defvar *cache-storage-lock* (bordeaux-threads:make-lock "*cache-storage*"))

(defun cached (f key &key (duration 1))
  (unless duration (return-from cached (funcall f)))
  (let* ((data (with-lock-held (*cache-storage-lock*)
			       (gethash key *cache-storage*)))
	 (mtime (first data))
	 (val (second data))
	 (now (get-universal-time))
	 )
    (if (and
	  mtime
	  (<= (- now mtime) duration))
      (progn
	val)
      (progn
	(setf val (funcall f))
	(with-lock-held (*cache-storage-lock*)
			(setf (gethash key *cache-storage*)
			      (list now val)))
	val
	))))

(defmacro cached-expr (e key &key 
  (duration 1)
  (eval-key nil)
  )
  (let* ((sym (gensym)))
        `(cached (lambda () ,e) 
                 (list ',sym ,(if eval-key key `',key))
                 :duration ,duration)
        ))

(defmacro mk-pair-generator (var lister &optional contents)
  ``(:type :generate
           :parser ,(lambda (,var) 
		      (find ,var
			    (let
			      (
			       (,var (list ,var))
			       )
			      ,lister)
			      :test 'equal 
			      :key 'first))
           :predicate ,(lambda (,var) 
                               (find (first ,var)
                                     ,lister
                                     :test 'equal
                                     :key 'first))
           :lister ,(lambda () 
		      (let 
			(
			 (,var (list nil))
			 ) 
			 ,lister))
           :contents ,,(if contents 
                           `(lambda (,var) ,contents)
                           `(lambda (,var) (second ,var)))))

(defmacro mk-creator (var create-file create-dir)
  `(list 
     :type :creator
     ,@(when create-file 
	 (list :create-file `(lambda (,var) ,create-file)))
     ,@(when create-dir (list :create-dir `(lambda (,var) ,create-dir)))
     ))

(defmacro mk-symlinker (var target-var &rest body)
  ;(fuse-complain "symlinker: ~s ~s ~s~%" var target-var body)
  `(list
     :type :symlinker
     :function (lambda (,var ,target-var) ,@body)
     ))

(defmacro mk-dir-remover (body)
  `(list
     :type :remover
     :function (lambda () ,body)))

(defmacro fmt (s var)
  `(format nil ,s ,var))

(defmacro id (&rest x) `(values ,@x))

(defun safe-call (f &rest args)
  (ignore-errors (return-from safe-call (values (apply 'funcall f args) t)))
  (values nil nil))

(defvar *object-cache-duration* nil)
(defvar *object-cache* (make-hash-table :test 'equal))
(defvar *object-cache-lock* (bordeaux-threads:make-lock "*object-cache*"))

(defun object-cache-retrieve (key now)
  ;(fuse-complain "Cache-search: ~s" key)
  (let* ((data 
	   (with-lock-held
	     (*object-cache-lock*)
	     (gethash key *object-cache*)))
	 (mtime (car data))
	 )
    (if (and
	  mtime 
	  (<= (- now mtime) *object-cache-duration*))
      (progn
	;(fuse-complain "Cache hit: ~s" key data)
	(values (second data) t))
      (progn
	;(fuse-complain "Cache miss: ~s" key)
	(values nil nil)))))
        
(defun get-object (path initial &optional (rev-path-already nil))
  ;(fuse-complain "Getting object ~s from ~s (cache: ~s)" path initial *object-cache-duration*)
  (let*
    ((cache-processing (and *object-cache-duration*
			    (getf initial :root)))
     ;(dummy-x0 (fuse-complain "Cache in use? ~s" cache-processing))
     (rev-path (when *object-cache-duration* (reverse path)))
     ;(dummy-x1 (when cache-processing (fuse-complain "path reversed")))
     (now (when *object-cache-duration* (get-universal-time)))
     ;(dummy-x2 (when cache-processing (fuse-complain "time got")))
     (dummy (when cache-processing 
	      ;(fuse-complain "Cache-loop..")
	      (iter (for p on rev-path)
		    (multiple-value-bind 
		      (val success) 
		      (object-cache-retrieve 
			p now)
		      (when success
			(return-from 
			  get-object 
			  (get-object 
			    (subseq 
			      path
			      (length p))
			    val
			    p
			    ))
			)
		      )
		    )))
     (val
       (block
	 get-object-cached
	 ;(fuse-complain "Final retrieval")
	 (cond 
	   ((null path) initial)
	   ((eq (getf initial :type) :error) initial)
	   ((eq (getf initial :type) :file) 
	    `(:type :error :contents ,cl-fuse:error-ENOTDIR))
	   (t (let* 
		((the-name (car path)))
		(iter (for entry in (funcall (getf initial :contents)))
		      (case (getf entry :type)
			((:file :dir :symlink) 
			 (when (equal (getf entry :name)
				      the-name)
			   (return-from get-object-cached
					(get-object (cdr path)
						    entry))))
			(:generate
			  (multiple-value-bind 
			    (val parsed)
			    (safe-call (getf entry :parser) the-name)
			    (when 
			      (and parsed 
				   (safe-call (getf entry :predicate) val))
			      (return-from get-object-cached
					   (get-object 
					     (cdr path)
					     (funcall (getf entry :contents)
						      val))))))
			(t nil)))
		`(:type :error :contents ,cl-fuse:error-ENOENT)
		)))))
     )
    (declare (ignore dummy))
    (when (and rev-path *object-cache-duration*)
      (with-lock-held 
	(*object-cache-lock*)
	(setf (gethash (append rev-path rev-path-already ) *object-cache*)
	      (list now val))))
    ;(fuse-complain "Object got: ~s ~s" path val)
    val
    ))

(defun list-directory (obj &key (path nil path-given-p))
  (let* ((listing-type (getf obj :type))
         (listing-data (case listing-type
                             (:dir (funcall (getf obj :contents)))
                             (:generate (funcall (getf obj :lister)))
                             (t nil)))
         (entries (case listing-type 
                        (:dir listing-data)
                        (:generate (mapcar (getf obj :contents)
                                           listing-data))
                        (t nil)))
	 (now (when path-given-p (get-universal-time)))
         )
        (fuse-complain "Listing directory at path ~s~%Object is ~s~%Entries are ~{~s~%~}~%" path obj entries)
        (iter (for entry in entries)
              (iter:with result = nil)
	      (iter:with rev-path := (reverse path))
	      (iter:for et := (getf entry :type))
	      ;(fuse-complain "Looking at entry ~s~%Type is ~s~%" entry et)
	      (when (and 
		      *object-cache-duration* path-given-p
		      (or (eq et :file) (eq et :dir) (eq et :symlink)))
		;(fuse-complain "Pushing into cache ~s ~s" entry rev-path)
		(with-lock-held 
		  (*object-cache-lock*)
		  (setf (gethash (cons (getf entry :name) rev-path ) *object-cache*)
			(list now entry))))
              (case et
                    ((:dir :symlink) 
                     (push (list (getf entry :name) (getf entry :type))
			   result))
		    ((:file)
                     (push (getf entry :name) result))
                    (:generate
                     (setf result (append 
				    (if 
				      path-given-p
				      (list-directory entry :path path) 
				      (list-directory entry))
				    result)))
                    (t nil))
	      ;(fuse-complain "Listed directory ~s, result is~%~{~s~%~}~%----- END OF LISTING ----" path result)
              (finally (return result)))))

(defun launch-creator (obj kind name)
  (when (eq (getf obj :type) :dir)
        (iter (for entry in (funcall (getf obj :contents)))
              (for func := (getf entry kind))
              (for e-type := (getf entry :type))
              (when 
               (and (eq e-type :creator)
                    func
                    (funcall func name)
                    )
               (return t)
               )
              )))

(defun get-dir-remover (obj)
  (when (eq (getf obj :type) :dir)
    (iter
      (for entry in (funcall (getf obj :contents)))
      (for e-type := (getf entry :type))
      (for func := (getf entry :function))
      (when 
	(and
	  (eq e-type :remover)
	  func)
	(return func)))))

(defun launch-symlinker (obj name content)
  (when (eq (getf obj :type) :dir)
    (iter (for entry in (funcall (getf obj :contents)))
	  (for func := (getf entry :function))
	  (for e-type := (getf entry :type))
	  (when 
	    (and (eq e-type :symlinker)
		 func
		 (funcall func name content)
		 )
	    (return t)
	    )
	  )))

(defvar *description*)

(defmacro make-op (f args &rest body)
  `(defun ,f ,(append `(path) args `(&rest more-args))
          (declare (ignorable more-args))
	  ;(fuse-complain "make-op-created operation ~s called with path ~s~%" ',f path)
          (let* ((obj (get-object path *description*))
                 (result (progn ,@body))
                 )
                result
                )))

(make-op directoryp ()
	 ;(fuse-complain "Is-directory? ~s ~s" path obj)
  (eq (getf obj :type) :dir))

(make-op directory-content ()
	 ;(fuse-complain "List ~s ~s" path obj)
  (list-directory obj :path path))

(make-op symlinkp ()
  (eq (getf obj :type) :symlink))

(make-op symlink-target ()
  (when (eq (getf obj :type) :symlink)
      (funcall (getf obj :contents))))

(defvar *write-cache* (make-hash-table :test 'equal))
(defvar *write-cache-lock* (bordeaux-threads:make-lock "*write-cache*"))

(make-op file-read ()
 (when (eq (getf obj :type) :file)
   (or (with-lock-held (*write-cache-lock*) (gethash path *write-cache*))
       `(:offset 0 ,(funcall (getf obj :contents))))))

(make-op 
  file-size ()
  (when (eq (getf obj :type) :file)
    (let*
      (
       (size (getf obj :size))
       (content (and (not size) (funcall (getf obj :contents))))
       (content
	 (if (stringp content)
	   (cl-fuse::string-to-octets content :full-range)
	   content))
       )
      (if size (funcall size) (length content)))))

(make-op file-write-whole (data)
  (when (getf obj :writer)
        (with-lock-held
	  (*write-cache-lock*)
	  (setf (gethash path *write-cache*) data))
        (length data)
        ))

(make-op file-flush ()
  (let*
    (
     (wc (with-lock-held (*write-cache-lock*) (gethash path *write-cache*)))
     )
    (if 
      wc
      (progn
	(with-lock-held (*object-cache-lock*) 
			(remhash (reverse path) *object-cache*))
	(when
	  (getf obj :writer)
	  (funcall 
	    (getf obj :writer)
	    wc)))
      t
      )))

(make-op file-release ()
  (with-lock-held (*write-cache-lock*) (remhash path *write-cache*))
  t 
  )

(make-op file-unlink ()
  (when (getf obj :remover)
    (with-lock-held
      (*object-cache-lock*)
      (remhash (reverse path) *object-cache*)
      (remhash (cdr (reverse path)) *object-cache*))
    (funcall (getf obj :remover))))

(make-op dir-unlink ()
  (let*
    ((f (get-dir-remover obj)))
    (when f
      (with-lock-held
	(*object-cache-lock*)
	(remhash (reverse path) *object-cache*)
	(remhash (cdr (reverse path)) *object-cache*))
      (funcall f)
      )))

(defun safe-subseq (seq start end)
  (cond
   ((>= (length seq) end start)
    (subseq seq start end))
   ((= (length seq) 0) 
    (make-array (- end start) :element-type '(unsigned-byte 8)))
   ((>= end start)   
    (let* ((new-seq 
            (make-array (- end start) :element-type '(unsigned-byte 8))))
          (loop for i from start to (- (length seq) 1) 
                for j from 0 to (- end start 1)
                do
                (progn
                 (setf (elt new-seq j) (elt seq i))
                 )
                )
          new-seq
          ))
   (t nil)
   ))

(make-op file-truncate (offset)
  (with-lock-held
    (*write-cache-lock*)
    (setf
      (gethash path *write-cache*)
      (safe-subseq
	(gethash path *write-cache* 
		 (make-array 0 :element-type '(unsigned-byte 8)))
	0 offset)))
  t
  )

(make-op file-create ()
  (with-lock-held
    (*object-cache-lock*)
    (remhash (reverse path) *object-cache*))
  (launch-creator 
   (get-object (butlast path) *description*)
   :create-file
   (car (last path)))
  )

(make-op dir-create ()
  (with-lock-held
    (*object-cache-lock*)
    (remhash (reverse path) *object-cache*))
  (launch-creator 
   (get-object (butlast path) *description*)
   :create-dir
   (car (last path)))
  )

(make-op file-writeable-p ()
  (getf obj :writer))

(make-op file-executable-p ()
	 (getf obj :executable))

(make-op 
  symlink (content)
  (with-lock-held
    (*object-cache-lock*)
    (remhash (reverse path) *object-cache*))
  (launch-symlinker
    (get-object (butlast path) *description*)
    (car (last path))
    content))

(defun run-lisp-meta-fs (description &optional (target-path "/tmp/test") (call-manager nil)
  (thread-pool-size 8) (extra-fuse-args nil))
  (when call-manager (setf (pcall:thread-pool-size) thread-pool-size))
  (setf *description* `(:type :dir :contents ,(constantly description) :root t))
  (ignore-errors (ensure-directories-exist (concatenate 'string target-path "/")))
  (cl-fuse:fuse-run `("meta-fs" ,target-path 
                                "-o" "attr_timeout=1s" 
                                "-o" "entry_timeout=1s" 
                                "-o" "ac_attr_timeout=1s" 
                                "-o" "intr"
				,@ extra-fuse-args
				)
                    :directoryp #'directoryp
                    :directory-content #'directory-content
                    :symlink-target #'symlink-target
                    :symlinkp #'symlinkp
                    :file-read #'file-read
                    :file-size #'file-size
                    :file-write-whole #'file-write-whole
                    :file-writeable-p #'file-writeable-p
                    :file-executable-p #'file-executable-p
                    :file-flush #'file-flush
                    :file-release #'file-release
                    :truncate #'file-truncate
                    :file-create #'file-create
                    :mkdir #'dir-create
                    :unlink #'file-unlink
                    :rmdir  #'dir-unlink
		    :symlink #'symlink
                    :call-manager (if call-manager 
				      (let*
					(
					 (tasks nil)
					 (caller
					   (lambda 
					     (f &rest x) 
					     (declare (ignore x))
					     (loop
					       while (>= (length tasks) thread-pool-size)
					       do (let*
						    (
						     (first-finished (apply 'pcall:select-one tasks))
						     )
						    (when first-finished
						      (pcall:join first-finished)
						      (setf tasks (remove first-finished tasks)))
						    )
					       )
					     (push (pcall:pcall f) tasks)
					     ))
					 )
					caller)
                                      (lambda (f &rest x) 
                                              (declare (ignore x)) 
                                              (funcall f))
                                      )
                    ))
