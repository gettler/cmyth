;;;;
;;;; Copyright (C) 2012-2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(defpackage #:cmyth
  (:use #:cl #:cffi #:trivial-garbage)
  (:export
   ;; macros
   #:with-connection
   #:with-open-proginfo
   #:with-open-recording
   #:with-open-thumbnail
   #:with-recorded
   #:with-pending
   #:with-scheduled
   #:with-thumbnail-buffer
   #:with-recording-buffer
   #:with-file-buffer
   #:with-prog
   #:for-all-progs
   ;; refmem functions
   #:ref-refs
   #:ref-bytes
   ;; connection class
   #:new-connection
   #:protocol-version
   #:get-recorded #:get-pending #:get-scheduled
   #:storage-space-total #:storage-space-used #:get-event
   #:prog-count
   ;; proginfo class
   #:attr #:attr*
   #:open-file
   #:nth-prog
   ;; file class
   #:bytes #:seek #:offset #:read-bytes #:buflen
   ;; exception
   #:cmyth-error #:text
   ;; misc
   #:*cmyth-lock*
   #:*connection*
   #:*programs*
   #:*program*
   #:*file*
   ;; debug
   #:debug-all #:debug-none))

(in-package #:cmyth)

(cffi:define-foreign-library librefmem
    (t (:default "librefmem")))

(cffi:define-foreign-library libcmyth
    (t (:default "libcmyth")))

(cffi:use-foreign-library librefmem)
(cffi:use-foreign-library libcmyth)

(defvar *connection* nil)
(defvar *programs* nil)
(defvar *program* nil)
(defvar *file* nil)

(define-condition cmyth-error (error)
  ((text :initarg :text :reader text)))
