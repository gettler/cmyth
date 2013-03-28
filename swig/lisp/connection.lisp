;;;;
;;;; Copyright (C) 2012-2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(in-package #:cmyth)

(defclass connection ()
  ((host :initarg :host)
   (port :initarg :port :initform 6543)
   (buflen :initarg :buflen :initform 131072)
   (rcvbuf :initarg :rcvbuf :initform 4096)
   (conn :accessor conn)
   (econn :accessor econn))
  (:documentation "cmyth backend connection class"))

(defgeneric release (connection))
(defgeneric connect (connection))
(defgeneric protocol-version (connection))
(defgeneric get-progs (connection &key type))
(defgeneric storage-space (connection))
(defgeneric get-event (connection &optional number))

(defmethod release ((c connection))
  (let ((control (conn c))
	(event (econn c)))
    (setf (conn c) nil)
    (setf (econn c) nil)
    (ref_release control)
    (ref_release event)))

(defmethod connect ((c connection))
  (let ((cp (cmyth_conn_connect_ctrl (slot-value c 'host)
				     (slot-value c 'port)
				     (slot-value c 'buflen)
				     (slot-value c 'rcvbuf)))
	(ep (cmyth_conn_connect_event (slot-value c 'host)
				      (slot-value c 'port)
				      (slot-value c 'buflen)
				      (slot-value c 'rcvbuf))))
    (if (or (pointer-eq cp (null-pointer)) (pointer-eq ep (null-pointer)))
	nil
	(progn
	  (setf (conn c) cp)
	  (setf (econn c) ep)))))

(defmethod protocol-version ((c connection))
  (cmyth_conn_get_protocol_version (conn c)))

(defun morph-list (progs type)
  (case type
    ('array
     (make-array (list (length progs)) :initial-contents progs))
    ('hash-table
     (progn
       (let ((hash (make-hash-table :test 'equal)))
	 (loop for p in progs do
	      (setf (gethash (attr p :path-name) hash) p))
	 hash)))
    (otherwise progs)))

(defmethod get-progs ((c connection) &key (type nil))
  (let* ((plist (cmyth_proglist_get_all_recorded (conn c)))
	 (count (cmyth_proglist_get_count plist))
	 (progs (if (pointer-eq plist (null-pointer))
		    nil
		    (loop for i from 0 below count collect
			 (new-proginfo (conn c) plist i)))))
    (ref_release plist)
    (morph-list progs type)))

(defmethod storage-space ((c connection))
  (let ((total (foreign-alloc :long-long))
	(used (foreign-alloc :long-long))
	(ret))
    (cmyth_conn_get_freespace (conn c) total used)
    (setq ret (list (mem-ref total :long-long) (mem-ref used :long-long)))
    (foreign-free total)
    (foreign-free used)
    ret))

(defmethod get-event ((c connection) &optional (timeout nil))
  (with-foreign-pointer (tv 8)
    (let ((to (null-pointer)))
      (if timeout
	  (multiple-value-bind (seconds microseconds)
	      (truncate timeout)
	    (setf (mem-ref tv :int 0) seconds)
	    (setf (mem-ref tv :int 1) (truncate (* microseconds 1000000)))
	    (setf to tv)))
	(let ((rc (cmyth_event_select (econn c) to)))
	  (cond ((= rc (- 0 EINTR))
		 (return-from get-event (values nil nil)))
		((< rc 0)
		 (return-from get-event (values :CMYTH_EVENT_CLOSE nil)))
		((= rc 0)
		 (return-from get-event (values nil nil)))))))
  (let* ((len 512)
	 (data (foreign-alloc :char :count len))
	 (event (cmyth_event_get (econn c) data len)))
    (let ((message (foreign-string-to-lisp data)))
      (foreign-free data)
      (values event message))))

(defun storage-space-total (c)
  (nth 0 (storage-space c)))

(defun storage-space-used (c)
  (nth 1 (storage-space c)))

(defun new-connection (host &optional
		       (port 6543) (buflen 131072) (rcvbuf 4096))
  (let ((c (make-instance 'connection :host host :port port
			  :buflen buflen :rcvbuf rcvbuf)))
    (if (connect c)
	c
	(error 'exception :text "Connection failed"))))

(defmacro with-connection ((conn host) &body body)
  (let ((local (gensym)))
    `(let (,conn ,local)
       (unwind-protect
	    (progn
	      (setq ,local (new-connection ,host)
		    ,conn ,local)
	      ,@body)
	 (unless (null ,local)
	   (release ,local))))))

(defmacro with-progs ((progs conn &key (type nil)) &body body)
  (let ((local (gensym)))
    `(let (,progs ,local)
       (unwind-protect
	    (progn
	      (setq ,local (get-progs ,conn :type ,type)
		    ,progs ,local)
	      ,@body)
	 (unless (null ,local)
	   (bt:with-lock-held (*cmyth-lock*)
	     (for-all (p ,local)
		  (release p))))))))

(defun prog-count (progs)
  (if (typep progs 'hash-table)
      (hash-table-count progs)
      (length progs)))

(defmacro for-all ((p progs) &body body)
  `(flet ((for-one (,p)
	    ,@body))
     (if (typep ,progs 'list)
	 (mapc #'for-one ,progs)
	 (if (typep ,progs 'array)
	     (map nil #'for-one ,progs)
	     (if (typep ,progs 'hash-table)
		 (flet ((for-hash (k v)
			  (declare (ignore k))
			  (for-one v)))
		   (maphash #'for-hash ,progs))
		 (error 'exception :text "Invalid for-all data type!"))))))
