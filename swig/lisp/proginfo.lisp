;;;;
;;;; Copyright (C) 2012-2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(in-package #:cmyth)

(defclass proginfo ()
  ((pinfo :initarg :pinfo :accessor pinfo)
   (cbl :initarg :cbl :accessor cbl :initform nil)
   (hash :initarg :hash :accessor hash))
  (:documentation "cmyth program info class"))

(defgeneric release (proginfo))
(defgeneric copy (proginfo))
(defgeneric equals (proginfo proginfo2))
(defgeneric attr (proginfo &rest items))
(defgeneric attr* (proginfo &rest items))
(defgeneric port (proginfo))
(defgeneric seconds (proginfo))
(defgeneric bytes (proginfo))
(defgeneric start-string (proginfo))
(defgeneric end-string (proginfo))
(defgeneric open-file (proginfo &optional thumbnail))

(defun add-hash (p name
		 &optional (fname (find-symbol (string-upcase name) 'cmyth)))
  (setf (gethash name (hash p)) (funcall fname p)))

(defmethod release ((p proginfo))
  (ref_release (pinfo p))
  (if (cbl p)
      (ref_release (cbl p))))

(defmethod equals ((p1 proginfo) (p2 proginfo))
  (if (= (cmyth_proginfo_compare (pinfo p1) (pinfo p2)) 0)
      t
      nil))

(defmethod attr* ((p proginfo) &rest items)
  (loop for i in items collect
       (let ((str (gethash i (hash p))))
	 (if (null str)
	     (add-hash p i)
	     str))))

(defmethod attr ((p proginfo) &rest items)
  (let ((result (apply #'attr* p items)))
    (if (= 1 (length result))
	(values-list result)
	result)))

(defun get-string (func p)
  (let* ((str (funcall func (pinfo p)))
	 (ret (foreign-string-to-lisp str)))
    (ref_release str)
    ret))

(defmacro attr-string (name function)
  `(defgeneric ,name (proginfo))
  `(defmethod ,name ((p proginfo))
     (get-string ,function p)))

(defmethod port ((p proginfo))
  (cmyth_proginfo_port (pinfo p)))

(defmethod seconds ((p proginfo))
  (cmyth_proginfo_length_sec (pinfo p)))

(defmethod bytes ((p proginfo))
  (cmyth_proginfo_length (pinfo p)))

(attr-string description #'cmyth_proginfo_description)
(attr-string channel-name #'cmyth_proginfo_channame)
(attr-string path-name #'cmyth_proginfo_pathname)
(attr-string recording-group #'cmyth_proginfo_recgroup)
(attr-string subtitle #'cmyth_proginfo_subtitle)
(attr-string title #'cmyth_proginfo_title)

(defmacro time-string (p function)
  `(let* ((ts (funcall ,function (pinfo ,p)))
	  (tm (cmyth_timestamp_to_unixtime ts)))
     (with-foreign-object (tp :long)
       (setf (mem-aref tp :long) tm)
       (let ((str (c-ctime tp)))
	 (subseq str 0 (- (length str) 1))))))

(defmethod start-string ((p proginfo))
  (time-string p #'cmyth_proginfo_start))

(defmethod end-string ((p proginfo))
  (time-string p #'cmyth_proginfo_end))

(defmethod open-file ((p proginfo) &optional (thumbnail nil))
  (new-file (pinfo p) thumbnail))

(defun new-proginfo (conn plist which)
  (let* ((pinfo (cmyth_proglist_get_item plist which))
	 (hash (make-hash-table :test 'equal)))
    (if (pointer-eq pinfo (null-pointer))
	nil
	(make-instance 'proginfo :pinfo pinfo :cbl nil :hash hash))))

(defmethod copy ((p proginfo))
  (let ((progs (ref_hold (pinfo p)))
	(breaks (ref_hold (cbl p))))
    (make-instance 'proginfo :pinfo progs :cbl breaks :hash (hash p))))

(defmacro with-progs-reference ((copy progs &key (type nil)) &body body)
  (let ((local (gensym)))
    `(let (,copy ,local)
       (unwind-protect
	    (progn
	      (bt:with-lock-held (*cmyth-lock*)
		(setq ,local ,progs)
		(for-all (p ,local)
		     (ref_hold (pinfo p))
		     (let ((c (cbl p)))
		       (when c
			 (ref_hold c))))
		(setf ,copy ,local))
	      ,@body)
	 (unless (null ,local)
	   (bt:with-lock-held (*cmyth-lock*)
	     (for-all (p ,local)
		  (release p))))))))
