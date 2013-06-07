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

(defun add-hash (p name
		 &optional (fname (find-symbol (string-upcase name) 'cmyth)))
  (setf (gethash name (hash p)) (funcall fname p)))

(defmethod release ((p proginfo))
  (cancel-finalization p)
  (let ((pinfo (pinfo p))
	(cbl (cbl p)))
    (setf (pinfo p) nil)
    (setf (cbl p) nil)
    (setf (hash p) nil)
    (when pinfo
      (ref_release pinfo))
    (when cbl
      (ref_release pinfo))))

(defmethod equals ((p1 proginfo) (p2 proginfo))
  (if (= (cmyth_proginfo_compare (pinfo p1) (pinfo p2)) 0)
      t
      nil))

(defun attr* (p &rest items)
  (loop for i in items collect
       (let ((str (gethash i (hash p))))
	 (if (null str)
	     (add-hash p i)
	     str))))

(defun attr (&rest items)
  (let ((result (apply #'attr* *program* items)))
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
  `(defmethod ,name ((pa proginfo))
     (get-string ,function pa)))

(defmethod port ((p proginfo))
  (cmyth_proginfo_port (pinfo p)))

(defmethod seconds ((p proginfo))
  (cmyth_proginfo_length_sec (pinfo p)))

(defmethod bytes ((p proginfo))
  (cmyth_proginfo_length (pinfo p)))

(defmethod channel-id ((p proginfo))
  (cmyth_proginfo_chan_id (pinfo p)))

(attr-string description #'cmyth_proginfo_description)
(attr-string channel-name #'cmyth_proginfo_channame)
(attr-string channel-sign #'cmyth_proginfo_chansign)
(attr-string path-name #'cmyth_proginfo_pathname)
(attr-string recording-group #'cmyth_proginfo_recgroup)
(attr-string subtitle #'cmyth_proginfo_subtitle)
(attr-string title #'cmyth_proginfo_title)

(defmacro time-int (p function)
  `(let* ((ts (funcall ,function (pinfo ,p)))
	  (tm (cmyth_timestamp_to_unixtime ts)))
     (ref-release ts)
     tm))

(defmacro time-string (p function)
  `(let ((tm (time-int ,p ,function)))
     (with-foreign-object (tp :long)
       (setf (mem-aref tp :long) tm)
       (let ((str (c-ctime tp)))
	 (subseq str 0 (- (length str) 1))))))

(defmethod start-string ((p proginfo))
  (time-string p #'cmyth_proginfo_start))

(defmethod end-string ((p proginfo))
  (time-string p #'cmyth_proginfo_end))

(defmethod start ((p proginfo))
  (time-int p #'cmyth_proginfo_start))

(defmethod end ((p proginfo))
  (time-int p #'cmyth_proginfo_end))

(defmethod open-file ((p proginfo) &optional (thumbnail nil))
  (new-file (pinfo p) thumbnail))

(defun new-proginfo (conn plist which)
  (let* ((pinfo (cmyth_proglist_get_item plist which))
	 (hash (make-hash-table :test 'equal)))
    (if (pointer-eq pinfo (null-pointer))
	nil
	(let ((p (make-instance 'proginfo :pinfo pinfo :cbl nil :hash hash)))
	  (finalize p (lambda () (ref_release pinfo)))
	  p))))
