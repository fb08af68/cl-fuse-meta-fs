; Part of cl-fuse FUSE bindings for Common Lisp
; Distributed as free software under Lisp Library General Public License
; Distributed AS IS with NO WARRANTY

(asdf:defsystem :cl-fuse-meta-fs
  :author "Michael Raskin <fb08af68@rambler.ru>"
  :maintainer "Michael Raskin <fb08af68@rambler.ru>"
  :license "LLGPL"
  :description "CFFI bindings to FUSE (Filesystem in user space)"
  :name "cl-fuse-meta-fs"
  :depends-on (cl-fuse :iterate :pcall :bordeaux-threads)
  :serial t
  :components (
    (:file "lisp-meta-fs")
    ))
