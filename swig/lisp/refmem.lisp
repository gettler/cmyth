;;;; refmem.lisp

(in-package #:cmyth)

(defun get-usage ()
  (let ((items (foreign-alloc :int))
	(bytes (foreign-alloc :int))
	(ret))
    (ref_get_usage items bytes)
    (setq ret (list (mem-ref items :int) (mem-ref bytes :int)))
    (foreign-free items)
    (foreign-free bytes)
    ret))

(defun ref-refs ()
  (nth 0 (get-usage)))

(defun ref-bytes ()
  (nth 1 (get-usage)))

(defun ref-release (ptr)
  (ref_release ptr))
