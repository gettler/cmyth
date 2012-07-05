;;;; file.lisp

(in-package #:cmyth)

(defclass file ()
  ((f :initarg :file :accessor file)
   (buf :initarg :buf :accessor buf)
   (buflen :initarg :buflen :accessor buflen))
  (:documentation "cmyth file class"))

(defgeneric release (file))
(defgeneric bytes (file))
(defgeneric seek (file offset))
(defgeneric offset (file))
(defgeneric read-bytes (file))

(defmethod release ((f file))
  (foreign-free (buf f))
  (ref_release (file f)))

(defmethod bytes ((f file))
  (cmyth_file_length (file f)))

(defmethod seek ((f file) offset)
  (cmyth_file_seek (file f) offset 0))

(defmethod offset ((f file))
  (cmyth_file_seek (file f) 0 1))

(defmethod read-bytes ((f file))
  (let ((total (cmyth_file_request_block (file f) (buflen f)))
	(ptr (buf f))
	(n 0))
    (loop while (< n total) do
	 (let ((bytes (cmyth_file_get_block (file f) ptr (- total n))))
	   (if (< bytes 0)
	       (return-from read-bytes n))
	   (incf-pointer ptr bytes)
	   (incf n bytes)))
    n))

(defun new-file (p)
  (let* ((buflen 131072)
	 (buf (foreign-alloc :unsigned-char :count buflen))
	 (host (cmyth_proginfo_host p))
	 (port (cmyth_proginfo_port p))
	 (conn (cmyth_conn_connect_ctrl host port 16384 4096))
	 (file (cmyth_conn_connect_file p conn buflen buflen))
	 (ret nil))
    (if file
	(setf ret (make-instance 'file :file file :buf buf :buflen buflen)))
    (ref_release conn)
    (ref_release host)
    ret))

(defmacro with-open-program ((f p) &body body)
  (let ((local (gensym)))
    `(let (,f ,local)
       (unwind-protect
	    (progn (setq ,local (open-file ,p)
			 ,f ,local)
		   ,@body)
	 (unless (null ,local)
	   (release ,local))))))
