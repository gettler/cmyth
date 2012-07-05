;;;; proglist.lisp

(in-package #:cmyth)

(defclass proglist ()
  ((plist :initarg :plist :accessor plist)
   (conn :initarg :conn :accessor conn))
  (:documentation "cmyth program list class"))

(defgeneric release (proglist))
(defgeneric get-count (proglist))
(defgeneric get-prog (proglist which))

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

(defmacro with-proglist ((pl conn) &body body)
  (let ((local (gensym)))
    `(let (,pl ,local)
       (unwind-protect
	    (progn (setq ,local (new-proglist (conn ,conn))
			 ,pl ,local)
		   ,@body)
	 (unless (null ,local)
	   (release ,local))))))
