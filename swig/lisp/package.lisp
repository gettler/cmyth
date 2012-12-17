;;;; package.lisp

(defpackage #:cmyth
  (:use #:cl #:cffi)
  (:export
   ;; macros
   #:with-connection #:with-proglist #:with-progs #:with-proginfo
   #:with-open-program #:with-open-thumbnail
   ;; common methods
   #:release
   ;; refmem functions
   #:ref-refs #:ref-bytes #:ref-release
   ;; connection class
   #:new-connection #:protocol-version #:get-proglist #:get-progs
   #:storage-space-total #:storage-space-used #:get-event
   ;; proglist class
   #:get-count #:get-prog
   ;; proginfo class
   #:attr #:attr*
   #:open-file
   ;; file class
   #:bytes #:seek #:offset #:read-bytes #:buf #:buflen
   ;; exception
   #:exception #:text))

(in-package #:cmyth)

(cffi:define-foreign-library librefmem
    (t (:default "librefmem")))

(cffi:define-foreign-library libcmyth
    (t (:default "libcmyth")))

(cffi:use-foreign-library librefmem)
(cffi:use-foreign-library libcmyth)
