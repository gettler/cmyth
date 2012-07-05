;;;; cmyth.asd
;;;; Automatically generated.  Do not edit.

(asdf:defsystem #:cmyth
  :serial t
  :description "An API for communicating with a MythTV backend."
  :author "Jon Gettler <gettler@mvpmc.org>"
  :license "LGPL"
  :depends-on (
      #:cffi
  )
  :components (
    (:file "package")
    (:file "cmyth")
    (:file "refmem")
    (:file "exception")
    (:file "connection")
    (:file "file")
    (:file "proginfo")
    (:file "proglist")
  ))
