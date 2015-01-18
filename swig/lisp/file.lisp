;;;;
;;;; Copyright (C) 2012-2015, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(in-package #:cmyth)

(defclass file ()
  ((f :initarg :file :accessor file)
   (s-buffer :initarg :s-buffer :accessor s-buffer)
   (buflen :initarg :buflen :accessor buflen))
  (:documentation "cmyth file class"))

(defmethod release ((f file))
  (ref_release (file f)))

(defmethod bytes ((f file))
  (cmyth_file_length (file f)))

(defun seek (offset &optional (f *file*))
  (cmyth_file_seek (file f) offset 0))

(defmethod offset ((f file))
  (cmyth_file_seek (file f) 0 1))

(defun file-read-bytes (f bytes)
  (let ((buf (s-buffer f))
	(total (cmyth_file_request_block (file f) bytes))
	(n 0))
    (cffi:with-pointer-to-vector-data (ptr buf)
      (loop while (< n total) do
	   (let ((bytes (cmyth_file_get_block (file f) ptr (- total n))))
	     (if (< bytes 0)
		 (if (\= bytes (- 0 EINTR))
		     (return-from file-read-bytes n))
		 (progn
		   (cffi:incf-pointer ptr bytes)
		   (incf n bytes))))))
    n))

(defun read-bytes (&optional (f *file*))
  (let ((bytes (file-read-bytes f (buflen f))))
    (if (= bytes (buflen f))
	(s-buffer f)
	(make-array bytes :element-type '(unsigned-byte 8)
		    :initial-contents (subseq (s-buffer f) 0 bytes)))))

(defun new-file (p &optional (thumbnail nil))
  (let* ((buflen 131072)
	 (buf (make-array buflen :element-type '(unsigned-byte 8)))
	 (s-buf (cffi:make-shareable-byte-vector buflen))
	 (host (cmyth_proginfo_host p))
	 (port (cmyth_proginfo_port p))
	 (conn (cmyth_conn_connect_ctrl host port 16384 4096))
	 (file (if thumbnail
		   (cmyth_conn_connect_thumbnail p conn buflen buflen)
		   (cmyth_conn_connect_file p conn buflen buflen)))
	 (ret nil))
    (if file
	(setf ret (make-instance 'file :file file
				 :buflen buflen :s-buffer s-buf)))
    (ref_release conn)
    (ref_release host)
    ret))

(defmacro with-file-buffer ((buf bytes &optional (which nil)) &body body)
  (let ((local (gensym))
	(size (gensym)))
    `(flet ((get-proginfo (which)
	      (if (typep which 'proginfo)
		  which
		  (if (typep which 'integer)
		      (nth-prog which *programs*)
		      (gethash which *programs*)))))
       (let* ((,local (if ,which (get-proginfo ,which) *program*))
	      (,size (if (= ,bytes 0)
			 131072
			 ,bytes))
	      (,buf (make-array ,size
				:element-type '(unsigned-byte 8)
				:fill-pointer 0)))
	 (let ((*program* ,local))
	   ,@body)))))

(defmacro with-open-proginfo ((p thumb) &body body)
  (let ((local (gensym))
	(program (gensym)))
    `(let (,local
	   (,program (if ,p ,p *program*)))
       (unwind-protect
	    (flet ((get-proginfo (which)
		     (if (typep which 'proginfo)
			 which
			 (if (typep which 'integer)
			     (nth-prog which *programs*)
			     (gethash which *programs*)))))
	      (setf ,local (open-file (get-proginfo ,program) ,thumb))
	      (let ((*file* ,local))
		,@body))
	 (unless (null ,local)
	   (release ,local))))))

(defmacro with-open-recording ((&optional (p nil)) &body body)
  `(with-open-proginfo (,p nil)
     ,@body))

(defmacro with-open-thumbnail ((&optional (p nil)) &body body)
  `(with-open-proginfo (,p t)
     ,@body))

(defmacro with-thumbnail-buffer ((buf &optional (p nil)) &body body)
  (let ((program (gensym)))
    `(let ((,program (if ,p ,p *program*)))
       (with-file-buffer (file-buffer 1048576 ,program)
	 (with-open-thumbnail (,program)
	   (seek 0)
	   (let ((buflen (buflen *file*))
		 (size 0))
	     (dotimes (i 5)
	       (let* ((data (read-bytes))
		      (len (array-dimension data 0)))
		 (if (> len 0)
		     (progn
		       (incf size len)
		       (setf (fill-pointer file-buffer) size)
		       (dotimes (j len)
			 (setf (aref file-buffer (+ j (* i buflen)))
			       (aref data j)))))))
	     (let ((,buf (make-array size :element-type '(unsigned-byte 8)
				     :initial-contents file-buffer
				     :fill-pointer size)))
	       ,@body)))))))

(defmacro with-recording-buffer ((buf &optional (p *program*)) &body body)
  `(with-file-buffer (file-buffer 0 ,p)
     (with-open-recording (*program*)
       (let ((,buf file-buffer))
	 ,@body))))
