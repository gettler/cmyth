;;;;
;;;; Copyright (C) 2012-2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(in-package #:cmyth)

(defun get-usage ()
  (let ((items (foreign-alloc :int))
	(bytes (foreign-alloc :int))
	(ret))
    (ref_get_usage items bytes)
    (setq ret (list (mem-ref items :int) (mem-ref bytes :int)))
    (foreign-free items)
    (foreign-free bytes)
    ret))

(defun ref-refs ()
  (nth 0 (get-usage)))

(defun ref-bytes ()
  (nth 1 (get-usage)))

(defun ref-release (ptr)
  (ref_release ptr))
