;;;;
;;;;  Copyright (C) 2012, Jon Gettler
;;;;  http://www.mvpmc.org/
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;

(require 'md5)
(require 'cffi)

(use-package :cmyth)

(defun test-host (host)
  (handler-case
      (with-connection (conn host)
	(with-progs (progs conn)
	  (format t "Protocol Version: ~A~%" (protocol-version conn))
	  (format t "Recording Count: ~A~%" (length progs))
	  (format t "Storage space total: ~A  used: ~A~%"
		  (storage-space-total conn) (storage-space-used conn))
	  (loop for p in progs do
	       (format t "  ~A - ~A~%" (item p 'title) (item p 'subtitle))
	       (format t "    ~A - ~A~%" (item p 'pathname) (item p 'bytes))
	       (format t "    ~A~%" (item p 'description)))))
    (exception (e)
      (format t "Exception: ~A~%" (text e)))))

(defun test-file (host)
  (handler-case
      (with-connection (conn host)
	(with-proglist (pl conn)
	  (with-proginfo (p pl 0)
	    (with-open-program (f p)
	      (seek f 0)
	      (let* ((m (md5:make-md5-state))
		     (buflen (buflen f))
		     (b (make-array (* 5 buflen)
				    :element-type '(unsigned-byte 8))))
		(dotimes (i 5)
		  (let ((len (read-bytes f)))
		    (dotimes (j len)
		      (setf (aref b (+ j (* i buflen)))
			    (cffi:mem-aref (buf f) :unsigned-char j)))))
		(format t "MD5: ")
		(md5:update-md5-state m b)
		(loop for x across (md5:finalize-md5-state m) do
		     (format t "~(~X~)" x))
		(format t "~%"))))))
    (exception (e)
      (format t "Exception: ~A~%" (text e)))))

(defvar *host*
  (nth 1
       #+sbcl *posix-argv*
       #+clisp ext:*args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))))

(if (eq *host* nil)
    (setf *host* "localhost"))

(test-host "nosuchhost")
(test-host *host*)
(test-file *host*)

(format t "Refs:  ~A~%" (get-refs))
(format t "Bytes: ~A~%" (get-bytes))
