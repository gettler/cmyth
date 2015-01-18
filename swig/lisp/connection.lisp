;;;;
;;;; Copyright (C) 2012-2015, Jon Gettler
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
   (conn :accessor conn :initform nil)
   (econn :accessor econn :initform nil))
  (:documentation "cmyth backend connection class"))

(defmethod release ((c connection))
  (cancel-finalization c)
  (let ((control (conn c))
	(event (econn c)))
    (setf (conn c) nil)
    (setf (econn c) nil)
    (ref_release control)
    (ref_release event)))

(defmethod connect ((c connection))
  (let ((control (conn c))
	(event (econn c)))
    (cancel-finalization c)
    (when control
      (ref_release control))
    (when event
      (ref_release event)))
  (let ((cp (cmyth_conn_connect_ctrl (slot-value c 'host)
				     (slot-value c 'port)
				     (slot-value c 'buflen)
				     (slot-value c 'rcvbuf)))
	(ep (cmyth_conn_connect_event (slot-value c 'host)
				      (slot-value c 'port)
				      (slot-value c 'buflen)
				      (slot-value c 'rcvbuf))))
    (if (or (pointer-eq cp (null-pointer)) (pointer-eq ep (null-pointer)))
	(progn
	  (when cp
	    (ref_release cp))
	  (when ep
	    (ref_release ep))
	  nil)
	(progn
	  (setf (conn c) cp)
	  (setf (econn c) ep)))))

(defun protocol-version (&optional (c *connection*))
  (cmyth_conn_get_protocol_version (conn c)))

(defun morph-list (progs type)
  (let ((type-array 'array)
	(type-list 'list)
	(type-hash-table 'hash-table))
    (if (or (equal type 'array)
	    (equal type nil))
       (make-array (list (length progs)) :initial-contents progs)
       (if (equal type 'hash-table)
	   (let ((hash (make-hash-table :test 'equal)))
	     (loop for p in progs do
		  (setf (gethash (values-list (attr* p :path-name)) hash) p))
	     hash)
	   (if (equal type 'list)
	       progs
	       (error 'cmyth-error :text "unknown data type"))))))

(defmacro get-progs (c func type)
  `(let* ((plist (funcall ,func (conn ,c)))
	  (count (cmyth_proglist_get_count plist))
	  (progs (if (pointer-eq plist (null-pointer))
		     nil
		     (loop for i from 0 below count collect
			  (new-proginfo (conn ,c) plist i)))))
     (ref_release plist)
     (morph-list progs ,type)))

(defun get-recorded (&key (connection *connection*) (type nil))
  (get-progs connection 'cmyth_proglist_get_all_recorded type))

(defun get-pending (&key (connection *connection*) (type nil))
  (get-progs connection 'cmyth_proglist_get_all_pending type))

(defun get-scheduled (&key (connection *connection*) (type nil))
  (get-progs connection 'cmyth_proglist_get_all_scheduled type))

(defun storage-space (&optional (c *connection*))
  (let ((total (foreign-alloc :long-long))
	(used (foreign-alloc :long-long))
	(ret))
    (cmyth_conn_get_freespace (conn c) total used)
    (setq ret (list (mem-ref total :long-long) (mem-ref used :long-long)))
    (foreign-free total)
    (foreign-free used)
    ret))

(defun get-event (&key (connection *connection*) (timeout nil))
  (with-foreign-pointer (tv 8)
    (let ((to (null-pointer)))
      (if timeout
	  (multiple-value-bind (seconds microseconds)
	      (truncate timeout)
	    (setf (mem-ref tv :int 0) seconds)
	    (setf (mem-ref tv :int 1) (truncate (* microseconds 1000000)))
	    (setf to tv)))
	(let ((rc (cmyth_event_select (econn connection) to)))
	  (cond ((= rc (- 0 EINTR))
		 (return-from get-event (values nil nil)))
		((< rc 0)
		 (return-from get-event (values :CMYTH_EVENT_CLOSE nil)))
		((= rc 0)
		 (return-from get-event (values nil nil)))))))
  (let* ((len 512)
	 (data (foreign-alloc :char :count len))
	 (event (cmyth_event_get (econn connection) data len)))
    (let ((message (foreign-string-to-lisp data)))
      (foreign-free data)
      (values event message))))

(defun storage-space-total (&optional (c *connection*))
  (nth 0 (storage-space c)))

(defun storage-space-used (&optional (c *connection*))
  (nth 1 (storage-space c)))

(defun new-connection (host &optional
		       (port 6543) (buflen 131072) (rcvbuf 4096))
  (let ((c (make-instance 'connection :host host :port port
			  :buflen buflen :rcvbuf rcvbuf)))
    (if (connect c)
	(progn
	  (finalize c (lambda ()
			(ref_release (conn c))
			(ref_release (econn c))))
	  c)
	(error 'cmyth-error :text "connection failed"))))

(defmacro with-connection ((host) &body body)
  (let ((local (gensym)))
    `(let (,local)
       (unwind-protect
	    (progn
	      (setq ,local (new-connection ,host))
	      (let ((*connection* ,local))
		,@body))
	 (unless (null ,local)
	   (release ,local))))))

(defmacro with-progs ((func conn type) &body body)
  (let ((local (gensym)))
    `(let (,local)
       (unwind-protect
	    (let ((*connection* ,conn))
	      (setq ,local (funcall ,func :type ,type))
	      (let ((*programs* ,local))
		,@body))
	 (unless (null ,local)
	   (for-all-progs (,local)
			  (release *program*)))))))

(defmacro with-recorded ((&key (connection nil) (type nil))
			 &body body)
  (let ((local (gensym)))
    `(let ((,local (if ,connection ,connection *connection*)))
       (with-progs ('get-recorded ,local ,type)
	 ,@body))))

(defmacro with-pending ((&key (connection nil) (type nil))
			&body body)
  (let ((local (gensym)))
    `(let ((,local (if ,connection ,connection *connection*)))
       (with-progs ('get-pending ,local ,type)
	 ,@body))))

(defmacro with-scheduled ((&key (connection nil) (type nil))
			  &body body)
  (let ((local (gensym)))
    `(let ((,local (if ,connection ,connection *connection*)))
       (with-progs ('get-scheduled ,local ,type)
	 ,@body))))

(defun prog-count (&optional (progs *programs*))
  (if (typep progs 'hash-table)
      (progn
	(format t "prog-count: progs is a hash-table~%")
	(hash-table-count progs))
      (length progs)))

(defmacro for-all-progs ((&optional (programs nil)) &body body)
  (let ((local (gensym)))
    `(let ((,local (if ,programs ,programs *programs*)))
       (flet ((for-one (p)
		(let ((*program* p))
		  ,@body)))
	 (if (typep ,local 'list)
	     (mapc #'for-one ,local)
	     (if (typep ,local 'array)
		 (map nil #'for-one ,local)
		 (if (typep ,local 'hash-table)
		     (flet ((for-hash (k v)
			      (declare (ignore k))
			      (for-one v)))
		       (maphash #'for-hash ,local))
		     (error 'cmyth-error :text "invalid data type"))))))))

(defun nth-hash (table n)
  (let ((i 0)
	(item nil))
    (flet ((access (k v)
	     (declare (ignore k))
	     (if (= i n)
		 (setf item v))
	     (incf i)))
      (maphash #'access table))
    item))

(defun nth-prog (which &optional (programs *programs*))
  (if (typep programs 'list)
      (nth which programs)
      (if (typep programs 'hash-table)
	  (nth-hash programs which)
	  (aref programs which))))

(defmacro with-prog ((which &optional (programs nil)) &body body)
  (let ((pl (gensym))
	(p (gensym)))
    `(let* ((,pl (if ,programs ,programs *programs*))
	    (,p (if (typep ,pl 'hash-table)
		    (gethash ,which ,pl)
		    (nth-prog ,which ,pl))))
       (if ,p
	   (let ((*programs* ,pl)
		 (*program* ,p))
	     (progn
	       ,@body))
	   (error 'cmyth-error :text "invalid program")))))
