;;;;
;;;;  Copyright (C) 2012, Jon Gettler
;;;;  http://www.mvpmc.org/
;;;;
;;;;  This library is free software; you can redistribute it and/or
;;;;  modify it under the terms of the GNU Lesser General Public
;;;;  License as published by the Free Software Foundation; either
;;;;  version 2.1 of the License, or (at your option) any later version.
;;;;
;;;;  This library is distributed in the hope that it will be useful,
;;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;  Lesser General Public License for more details.
;;;;
;;;;  You should have received a copy of the GNU Lesser General Public
;;;;  License along with this library; if not, write to the Free Software
;;;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;;;

;;;;
;;;; cmyth package
;;;;

(require 'asdf)
(require 'cffi)

(defpackage :cmyth
  (:use :common-lisp :cffi)
  (:export
   ;; common methods
   :release
   ;; refmem functions
   :get-refs :get-bytes :ref-release
   ;; connection class
   :new-connection :protocol-version :get-proglist
   :storage-space-total :storage-space-used
   ;; proglist class
   :get-count :get-prog
   ;; proginfo class
   :title :subtitle :description
   :open-file
   ;; file class
   :bytes :seek :offset :read-bytes :buf
   ))

(in-package :cmyth)

(define-foreign-library librefmem
    (t (:default "librefmem")))

(define-foreign-library libcmyth
    (t (:default "libcmyth")))

(use-foreign-library librefmem)
(use-foreign-library libcmyth)

(load (format nil "~A~A"
	      #+sbcl
	      (sb-ext:posix-getenv "LISPDIR")
	      #+clisp
	      (ext:getenv "LISPDIR")
	      #+ecl
	      (si:getenv "LISPDIR")
	      "/cmyth_cffi.lisp"))

;;;
;;; refmem functions
;;;
(defun get-usage ()
  (let ((items (foreign-alloc :int))
	(bytes (foreign-alloc :int))
	(ret))
    (ref_get_usage items bytes)
    (setq ret (list (mem-ref items :int) (mem-ref bytes :int)))
    (foreign-free items)
    (foreign-free bytes)
    ret))

(defun get-refs ()
  (nth 0 (get-usage)))

(defun get-bytes ()
  (nth 1 (get-usage)))

(defun ref-release (ptr)
  (ref_release ptr))

;;;
;;; file class
;;;
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

;;;
;;; proginfo class
;;;
(defclass proginfo ()
  ((pinfo :initarg :pinfo :accessor pinfo)
   (cbl :initarg :cbl :accessor cbl :initform nil))
  (:documentation "cmyth program info class"))

(defgeneric release (proginfo))
(defgeneric equals (proginfo proginfo))
(defgeneric port (proginfo))
(defgeneric seconds (proginfo))
(defgeneric bytes (proginfo))
(defgeneric title (proginfo))
(defgeneric subtitle (proginfo))
(defgeneric description (proginfo))
(defgeneric open-file (proginfo))

(defmethod release ((p proginfo))
  (ref_release (pinfo p))
  (ref_release (cbl p)))

(defmethod equals ((p1 proginfo) (p2 proginfo))
  (if (= (cmyth_proginfo_compare (pinfo p1) (pinfo p2)) 0)
      t
      nil))

(defun get-string (func p)
  (let* ((str (funcall func (pinfo p)))
	 (ret (foreign-string-to-lisp str)))
    (ref_release str)
    ret))

(defmethod title ((p proginfo))
  (get-string #'cmyth_proginfo_title p))

(defmethod subtitle ((p proginfo))
  (get-string #'cmyth_proginfo_subtitle p))

(defmethod description ((p proginfo))
  (get-string #'cmyth_proginfo_description p))

(defmethod open-file ((p proginfo))
  (new-file (pinfo p)))

(defun new-proginfo (conn plist which)
  (let* ((pinfo (cmyth_proglist_get_item plist which))
	 (cbl (cmyth_get_commbreaklist conn pinfo)))
    (if (pointer-eq pinfo (null-pointer))
	nil
	(make-instance 'proginfo :pinfo pinfo :cbl cbl))))

;;;
;;; proglist class
;;;
(defclass proglist ()
  ((plist :initarg :plist :accessor plist)
   (conn :initarg :conn :accessor conn))
  (:documentation "cmyth program list class"))

(defgeneric release (proglist))
(defgeneric get-count (proglist))
(defgeneric get-prog (proglist :int))

(defmethod release ((p proglist))
  (ref_release (plist p)))

(defmethod get-count ((p proglist))
  (cmyth_proglist_get_count (slot-value p 'plist)))

(defmethod get-prog ((p proglist) which)
  (new-proginfo (conn p) (plist p) which))

(defun new-proglist (conn)
  (let ((plist (cmyth_proglist_get_all_recorded conn)))
    (if (pointer-eq plist (null-pointer))
	nil
	(make-instance 'proglist :plist plist :conn conn))))

;;;
;;; connection class
;;;
(defclass connection ()
  ((host :initarg :host)
   (port :initarg :port :initform 6543)
   (buflen :initarg :buflen :initform 131072)
   (rcvbuf :initarg :rcvbuf :initform 4096)
   (conn :accessor conn))
  (:documentation "cmyth backend connection class"))

(defgeneric release (connection))
(defgeneric connect (connection))
(defgeneric protocol-version (connection))
(defgeneric get-proglist (connection))
(defgeneric storage-space (connection))

(defmethod release ((c connection))
  (ref_release (conn c)))

(defmethod connect ((c connection))
  (let ((cp (cmyth_conn_connect_ctrl (slot-value c 'host)
				     (slot-value c 'port)
				     (slot-value c 'buflen)
				     (slot-value c 'rcvbuf))))
    (if (pointer-eq cp (null-pointer))
	nil
	(setf (conn c) cp))))

(defmethod protocol-version ((c connection))
  (cmyth_conn_get_protocol_version (conn c)))

(defmethod get-proglist ((c connection))
  (new-proglist (conn c)))

(defmethod storage-space ((c connection))
  (let ((total (foreign-alloc :long-long))
	(used (foreign-alloc :long-long))
	(ret))
    (cmyth_conn_get_freespace (conn c) total used)
    (setq ret (list (mem-ref total :long-long) (mem-ref used :long-long)))
    (foreign-free total)
    (foreign-free used)
    ret))

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
	nil)))
