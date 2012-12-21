;;;; connection.lisp

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
(defgeneric get-proglist (connection))
(defgeneric get-progs (connection))
(defgeneric storage-space (connection))
(defgeneric get-event (connection &optional number))

(defmethod release ((c connection))
  (ref_release (conn c))
  (ref_release (econn c)))

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

(defmethod get-proglist ((c connection))
  (new-proglist (conn c)))

(defmethod get-progs ((c connection))
  (let* ((plist (cmyth_proglist_get_all_recorded (conn c)))
	 (progs (if (pointer-eq plist (null-pointer))
		    nil
		    (loop for i from 0 below
			 (cmyth_proglist_get_count plist) collect
			 (new-proginfo (conn c) plist i)))))
    (ref_release plist)
    progs))

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
	  (cond ((< rc 0)
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
	    (progn (setq ,local (new-connection ,host)
			 ,conn ,local)
		   ,@body)
	 (unless (null ,local)
	   (release ,local))))))

(defmacro with-progs ((progs conn) &body body)
  (let ((local (gensym)))
    `(let (,progs ,local)
       (unwind-protect
	    (progn (setq ,local (get-progs ,conn)
			 ,progs ,local)
		   ,@body)
	 (unless (null ,local)
	   (loop while (> (length ,local) 0) do
		(release (pop ,local))))))))

