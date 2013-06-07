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
#-ecl (require 'iolib)
(require 'cffi)
(require 'md5)

(pushnew #-ecl (iolib.syscalls:getenv "LISPDIR")
	 #+ecl (si:getenv "LISPDIR")
	 asdf:*central-registry* :test #'equal)

(require 'cmyth)

(use-package :cmyth)

(defun test-host (host)
  (handler-case
      (with-connection (host)
	(format t "Protocol Version: ~A~%" (protocol-version))
	(multiple-value-bind (type data)
	    (get-event)
	  (format t "Event: ~A \"~A\"~%" type data))
	(with-recorded (:type 'hash-table)
	  (format t "Recording Count: ~A~%" (prog-count))
	  (format t "Storage space total: ~A  used: ~A~%"
		  (storage-space-total) (storage-space-used))
	  (for-all-progs ()
	       (format t "  ~{~A~^ - ~}~%" (attr :title :subtitle))
	       (format t "    ~{~A~^ - ~}~%" (attr :path-name :bytes))
	       (format t "    ~{~A~^ - ~}~%" (attr :start :end))
	       (format t "    ~{~A~^ - ~}~%" (attr :start-string :end-string))
	       (format t "    ~{~A~^ - ~}~%" (attr :channel-name
						   :channel-sign :channel-id))
	       (format t "    ~A~%" (attr :description))))
	(with-pending (:type 'array)
	  (format t "Pending Count: ~A~%" (prog-count))
	  (for-all-progs ()
	       (format t "  ~{~A~^ - ~}~%" (attr :title :subtitle))
	       (format t "    ~{~A~^ - ~}~%" (attr :start-string :end-string))
	       (format t "    ~{~A~^ - ~}~%" (attr :channel-name
						   :channel-sign :channel-id))))
	(with-scheduled (:type 'list)
	  (format t "Scheduled Count: ~A~%" (prog-count))
	  (for-all-progs ()
	       (format t "  ~A~%" (attr :title)))))
    (cmyth-error (e)
      (format t "Exception: ~A~%" (text e)))))

(defun test-file (host)
  (handler-case
      (with-connection (host)
	(with-recorded ()
	  (with-open-recording (0)
	    (seek 0)
	    (let ((m (md5:make-md5-state)))
	      (dotimes (i 5)
		(let ((buf (read-bytes)))
		  (md5:update-md5-state m buf)))
	      (format t "MD5: ")
	      (loop for x across (md5:finalize-md5-state m) do
		   (format t "~(~2,'0X~)" x))
	      (format t "~%")))))
    (cmyth-error (e)
      (format t "Exception: ~A~%" (text e)))))

(defun test-thumbnail (host)
  (handler-case
      (with-connection (host)
	(with-recorded ()
	  (with-thumbnail-buffer (b 0)
	    (let ((m (md5:make-md5-state))
		  (size (fill-pointer b)))
	      (format t "Thumbnail image size: ~A~%" size)
	      (format t "MD5: ")
	      (md5:update-md5-state m (subseq b 0 size))
	      (format t "~{~a~^~}"
		      (loop for x across (md5:finalize-md5-state m) collect
			   (format nil "~(~2,'0X~)" x)))
	      (format t "~%")))))
    (cmyth-error (e)
      (format t "Exception: ~A~%" (text e)))))

(defmacro wall-clock-time (() &body body)
  `(let ((start (get-internal-real-time)))
     ,@body
     (float (/ (- (get-internal-real-time) start)
	       internal-time-units-per-second))))

(defun test-perf (host)
  (handler-case
      (with-connection (host)
	(with-recorded ()
	  (with-open-recording (0)
	    (let* ((offset 0)
		   (duration
		    (wall-clock-time ()
				     (loop while (< offset 67108864) do
					  (let* ((data (read-bytes))
						 (n (array-dimension data 0)))
					    (when (<= n 0)
					      (return))
					    (incf offset n))))))
	      (format t "Perf: read ~A bytes in ~A seconds (~A mb/s)~%"
		      offset duration
		      (/ (/ (* offset 8) duration) 1000000))))))
    (cmyth-error (e)
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
    (test-perf host)

    (format t "Refs:  ~A~%" (ref-refs))
    (format t "Bytes: ~A~%" (ref-bytes))))

(main)
