;;;; proginfo.lisp

(in-package #:cmyth)

(defclass proginfo ()
  ((pinfo :initarg :pinfo :accessor pinfo)
   (cbl :initarg :cbl :accessor cbl :initform nil)
   (hash :initarg :hash :accessor hash))
  (:documentation "cmyth program info class"))

(defgeneric release (proginfo))
(defgeneric equals (proginfo proginfo2))
(defgeneric attr (proginfo &rest items))
(defgeneric attr* (proginfo &rest items))
(defgeneric port (proginfo))
(defgeneric seconds (proginfo))
(defgeneric bytes (proginfo))
(defgeneric title (proginfo))
(defgeneric subtitle (proginfo))
(defgeneric description (proginfo))
(defgeneric path-name (proginfo))
(defgeneric open-file (proginfo))

(defun add-hash (p name
		 &optional (fname (find-symbol (string-upcase name) 'cmyth)))
  (setf (gethash name (hash p)) (funcall fname p)))

(defmethod release ((p proginfo))
  (ref_release (pinfo p))
  (ref_release (cbl p)))

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

(defmethod port ((p proginfo))
  (cmyth_proginfo_port (pinfo p)))

(defmethod seconds ((p proginfo))
  (cmyth_proginfo_length_sec (pinfo p)))

(defmethod bytes ((p proginfo))
  (cmyth_proginfo_length (pinfo p)))

(defmethod title ((p proginfo))
  (get-string #'cmyth_proginfo_title p))

(defmethod subtitle ((p proginfo))
  (get-string #'cmyth_proginfo_subtitle p))

(defmethod description ((p proginfo))
  (get-string #'cmyth_proginfo_description p))

(defmethod path-name ((p proginfo))
  (get-string #'cmyth_proginfo_pathname p))

(defmethod open-file ((p proginfo))
  (new-file (pinfo p)))

(defun new-proginfo (conn plist which)
  (let* ((pinfo (cmyth_proglist_get_item plist which))
	 (cbl (cmyth_get_commbreaklist conn pinfo))
	 (hash (make-hash-table :test 'equal)))
    (if (pointer-eq pinfo (null-pointer))
	nil
	(let ((p (make-instance 'proginfo :pinfo pinfo :cbl cbl :hash hash)))
	  p))))

(defmacro with-proginfo ((p proglist which) &body body)
  (let ((local (gensym)))
    `(let (,p ,local)
       (unwind-protect
	    (progn (setq ,local (new-proginfo (conn ,proglist) (plist ,proglist)
					      ,which)
			 ,p ,local)
		   ,@body)
	 (unless (null ,local)
	   (release ,local))))))
