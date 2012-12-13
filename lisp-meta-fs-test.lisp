; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(require :cl-fuse-meta-fs)
#+sbcl (require :sb-posix)
(require :iterate)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unexport 'iterate:terminate :iterate))

(use-package :cl-fuse-meta-fs)
(use-package :iterate)

#+nil (defvar *description* 
  `((:type :file :name "README" :contents ,(constantly "Hello here!"))
    (:type :symlink :name "your-tmp" :contents ,(constantly "/tmp"))
    (:type :symlink :name "your-tmp-too" :contents ,(constantly '("tmp")))
    (:type :dir :name "a-directory" :contents 
           ,(constantly
             `((:type :file :name "INFO" :contents ,(constantly "An example directory"))
               (:type :generate :parser parse-integer 
                      :predicate ,(lambda (x) (<= 1 x 10))
                      :lister ,(constantly '(1 2 3 4 5))
                      :contents ,(lambda (x) `(:type :file :name ,(format nil "~a" x)
                                                     :contents ,(lambda () (format nil "Number ~a" x))))))))))

(defun lndir-on (path)
  (mk-generator x identity (id t) 
                (mapcar (lambda (n)
                                (or (pathname-name n)
                                    (car (last (pathname-directory n)))))
                        (directory (concatenate 'string path "/*") 
                                   :resolve-symlinks nil))
                (let* ((full-path (concatenate 'string path "/" x))
                       (stat (ignore-errors #+sbcl (sb-posix:lstat full-path)))
                       )
                      (cond
                       ((not (probe-file (concatenate 'string path "/" x)))
                        (mk-symlink x (subseq 
                                         (concatenate 'string path "/" x)
                                         1)))
                       ((or nil #+sbcl (sb-posix:s-isdir (sb-posix:stat-mode stat)))
                        (mk-dir x :just
                                (lndir-on (concatenate 'string path "/" x))))
                       (t (mk-symlink x (subseq 
                                         (concatenate 'string path "/" x)
                                         1)))
                       ))))

(defvar *special-directory-description* nil)
(defvar *structured-description* (make-hash-table :test 'equal))

(defvar *description* 
  (list
   (mk-file "README" "Hello here!")
   (mk-symlink "your-tmp" "/tmp")
   (mk-symlink "your-tmp-too" (list "tmp"))
   (mk-dir "a-directory" :just
           (mk-file "INFO" "An example directory")
           (mk-generator x parse-integer (<= 1 x 10) 
                         (loop for n from 1 to 5 collect n)
                         (mk-file (fmt "~a" x)
                                  (fmt "Number ~a" x))))
   (mk-dir "sum" :just 
           (mk-generator x parse-integer (id t) nil
                         (mk-dir (fmt "~a" x) :just 
                                 (mk-generator y parse-integer (id t) nil
                                               (mk-file (fmt "~a" y)
                                                        (fmt "Sum is ~a~%"
                                                             (+ x y)))))))
   (mk-dir "lndir" :just
           (lndir-on "/"))
   (mk-dir 
    "packages" :just
    (mk-generator 
     pkg find-package find-package (list-all-packages)
     (mk-dir 
      (package-name pkg) :just
      (mk-file "::name" (package-name pkg))
      (mk-file "::nicknames" (fmt "~{~a~%~}" (package-nicknames pkg)))
      (mk-file "::shadowing-symbols" (fmt "~{~a~%~}" (package-shadowing-symbols pkg)))
      (mk-file "::uses" (fmt "~{~a~%~}" (package-use-list pkg)))
      (mk-file "::used-by" (fmt "~{~a~%~}" (package-used-by-list pkg)))
      (mk-generator 
       symb 
       (find-symbol symb pkg)
       identity
       (loop for x being each 
             present-symbol of pkg 
             collect x)
       (mk-dir 
        (symbol-name symb) :just
        (mk-file 
         "::contents"
         (fmt "~a" (ignore-errors (symbol-value symb)))
         (data
          (setf (symbol-value symb) 
                (with-input-from-string 
                 (s (cl-fuse::octets-to-string data :full-range))
                 (read s)))
          0
          )
         )
        (mk-file 
         "::documentation" 
         (with-output-to-string 
          (s) (describe symb s)))
        (mk-file 
         "::bound"
         (fmt "~a~%" (boundp symb))
         )
        (mk-file 
         "::function-bound"
         (fmt "~a~%" (fboundp symb))
         )
        )
       )
      )
     )
    )
   (mk-dir "your-code-result" :eval 
           (mapcar 'eval *special-directory-description*))
   (mk-file "your-code" (fmt "~s~%" *special-directory-description*)
            (data 
             (let* (
                    (str 
                     (cl-fuse::octets-to-string data :full-range))
                    (code 
                     (with-input-from-string 
                      (s str)
                      (read s)))
                    )
                   (setf *special-directory-description* code)
                   )))
   (mk-dir "test-pairs" :just
           (mk-pair-generator x `(("a" "File a") ("b" "File b")) 
                              (mk-file (first x) (second x)))
           )
   (mk-dir "structured-code-result" :just
           (mk-pair-generator 
            x (iter (for (n v) :in-hashtable *structured-description*)
                    (for entry := (eval `(let ((name ,n)) ,v)))
                    (collect (list n entry)))))
   (mk-dir "structured-code" :just
           (mk-pair-generator 
            x (iter (for (n v) :in-hashtable *structured-description*)
                    (collect (list n (fmt "~s~%" v))))
            (mk-file (first x) (second x) 
                     (data
                      (let* (
                             (str (cl-fuse::octets-to-string data :full-range))
                             (code (with-input-from-string 
                                    (s str)
                                    (ignore-errors (read s))))
                             )
                            (setf (gethash (first x) *structured-description*) 
                                  code)
                            ))
                     ))
           (mk-creator 
            name
            (when (not (equal ".sw" (subseq name 
					    (- (length name) 4) 
					    (- (length name) 1))))
                  (setf (gethash name *structured-description*) ""))
            nil))
   (mk-file "inject-code" ""
            (data
             (let* (
                    (str (cl-fuse::octets-to-string data :full-range))
                    (code (with-input-from-string 
                           (s str)
                           (ignore-errors (read s))))
                    )
                   (eval code)
                   )
             ))
   ))

(setf cl-fuse::*break-on-errors* t)

(cl-fuse:fuse-complain "FS is ~s~%" *description*)

(cl-fuse-meta-fs:run-lisp-meta-fs *description*)
