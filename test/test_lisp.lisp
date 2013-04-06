;;;;
;;;;  Copyright (C) 2012-2013, Jon Gettler
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

(require 'asdf)
(require 'iolib)
(require 'cffi)
(require 'md5)

(pushnew (iolib.syscalls:getenv "LISPDIR")
	 asdf:*central-registry* :test #'equal)

(require 'cmyth)

(use-package :cmyth)

(defun test-host (host)
  (handler-case
      (with-connection (conn host)
	(format t "Protocol Version: ~A~%" (protocol-version conn))
	(multiple-value-bind (type data)
	    (get-event conn)
	  (format t "Event: ~A \"~A\"~%" type data))
	(with-recorded (progs conn :type 'array)
	  (format t "Recording Count: ~A~%" (prog-count progs))
	  (format t "Storage space total: ~A  used: ~A~%"
		  (storage-space-total conn) (storage-space-used conn))
	  (for-all (p progs)
	       (format t "  ~{~A~^ - ~}~%" (attr p :title :subtitle))
	       (format t "    ~{~A~^ - ~}~%" (attr p :path-name :bytes))
	       (format t "    ~{~A~^ - ~}~%" (attr p :start :end))
	       (format t "    ~{~A~^ - ~}~%" (attr p :start-string :end-string))
	       (format t "    ~{~A~^ - ~}~%" (attr p :channel-name
						   :channel-sign :channel-id))
	       (format t "    ~A~%" (attr p :description))))
	(with-pending (progs conn :type 'array)
	  (format t "Pending Count: ~A~%" (prog-count progs))
	  (for-all (p progs)
	       (format t "  ~{~A~^ - ~}~%" (attr p :title :subtitle))
	       (format t "    ~{~A~^ - ~}~%" (attr p :start-string :end-string))
	       (format t "    ~{~A~^ - ~}~%" (attr p :channel-name
						   :channel-sign :channel-id))))
	(with-scheduled (progs conn :type 'array)
	  (format t "Scheduled Count: ~A~%" (prog-count progs))
	  (for-all (p progs)
	       (format t "  ~A~%" (attr p :title)))))
    (exception (e)
      (format t "Exception: ~A~%" (text e)))))

(defun test-file (host)
  (handler-case
      (with-connection (conn host)
	(with-recorded (progs conn)
	  (with-open-program (f (nth 0 progs))
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
		   (format t "~(~2,'0X~)" x))
	      (format t "~%")))))
    (exception (e)
      (format t "Exception: ~A~%" (text e)))))

(defun test-thumbnail (host)
  (handler-case
      (with-connection (conn host)
	(with-recorded (progs conn)
	  (with-open-thumbnail (f (nth 0 progs))
	    (seek f 0)
	    (let* ((m (md5:make-md5-state))
		   (buflen (buflen f))
		   (size 0)
		   (b (make-array (* 5 buflen)
				  :element-type '(unsigned-byte 8))))
	      (dotimes (i 5)
		(let ((len (read-bytes f)))
		  (if (> len 0)
		      (progn
			(incf size len)
			(dotimes (j len)
			  (setf (aref b (+ j (* i buflen)))
				(cffi:mem-aref (buf f) :unsigned-char j)))))))
	      (format t "Thumbnail image size: ~A~%" size)
	      (format t "MD5: ")
	      (md5:update-md5-state m (subseq b 0 size))
	      (format t "~{~a~^~}"
		      (loop for x across (md5:finalize-md5-state m) collect
			   (format nil "~(~2,'0X~)" x)))
	      (format t "~%")))))
    (exception (e)
      (format t "Exception: ~A~%" (text e)))))

(defun main ()
  (let ((host
	 (nth 1
	      #+sbcl *posix-argv*
	      #+clisp ext:*args*
	      #+ccl (list "ccl" (nth 6 (ccl::command-line-arguments)))
	      #+ecl (list "ecl" (si:argv (- (si:argc) 1))))))

    (if (eq host nil)
	(setf host "localhost"))

    (test-host "nosuchhost")
    (test-host host)
    (test-file host)
    (test-thumbnail host)

    (format t "Refs:  ~A~%" (ref-refs))
    (format t "Bytes: ~A~%" (ref-bytes))))

(main)
