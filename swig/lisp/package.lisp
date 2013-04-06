;;;;
;;;; Copyright (C) 2012-2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(defpackage #:cmyth
  (:use #:cl #:cffi)
  (:export
   ;; macros
   #:with-connection
   #:with-open-program #:with-open-thumbnail
   #:with-recorded #:with-pending #:with-scheduled
   #:with-progs-reference
   #:for-all
   ;; common methods
   #:release
   ;; refmem functions
   #:ref-refs #:ref-bytes #:ref-release
   ;; connection class
   #:new-connection #:protocol-version
   #:get-recorded #:get-pending #:get-scheduled
   #:storage-space-total #:storage-space-used #:get-event
   #:prog-count
   ;; proginfo class
   #:attr #:attr*
   #:open-file
   ;; file class
   #:bytes #:seek #:offset #:read-bytes #:buf #:buflen
   ;; exception
   #:exception #:text
   ;; misc
   #:*cmyth-lock*
   ;; debug
   #:debug-all #:debug-none))

(in-package #:cmyth)

(cffi:define-foreign-library librefmem
    (t (:default "librefmem")))

(cffi:define-foreign-library libcmyth
    (t (:default "libcmyth")))

(cffi:use-foreign-library librefmem)
(cffi:use-foreign-library libcmyth)

(defvar *cmyth-lock* (bt:make-lock "cmyth"))
