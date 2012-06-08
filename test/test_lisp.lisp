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
  (let* ((conn (new-connection host))
	 (pl (get-proglist conn)))
    (format t "Protocol Version: ~A~%" (protocol-version conn))
    (format t "Recording Count: ~A~%" (get-count pl))
    (format t "Storage space total: ~A  used: ~A~%"
	    (storage-space-total conn) (storage-space-used conn))
    (loop for i from 0 below (get-count pl) do
	 (let ((p (get-prog pl i)))
	   (format t "  ~A - ~A~%" (title p) (subtitle p))
	   (format t "    ~A~%" (description p))
	   (release p)))
    (release pl)
    (release conn)))

(defun test-file (host)
  (let* ((conn (new-connection host))
	 (pl (get-proglist conn))
	 (p (get-prog pl 0))
	 (f (open-file p)))
    (format t "bytes: ~A~%" (bytes f))
    (seek f 0)
    (let* ((m (md5:make-md5-state))
	   (buflen 131072)
	   (sum)
	   (b (make-array (* 5 buflen) :element-type '(unsigned-byte 8))))
      (dotimes (i 5)
	(let ((len (read-bytes f)))
	  (dotimes (j len)
	    (setf (aref b (+ j (* i buflen)))
		  (cffi:mem-aref (buf f) :unsigned-char j)))))
;      (with-open-file (stream "file.out" :element-type '(unsigned-byte 8)
;			      :direction :output :if-exists :supersede)
;	(write-sequence b stream))
      (format t "MD5: ")
      (setf m (md5:update-md5-state m b))
      (setf sum (md5:finalize-md5-state m))
      (loop for x across sum do
	   (format t "~(~X~)" x))
      (format t "~%"))
    (release f)
    (release p)
    (release pl)
    (release conn)))

(defvar *host*
  (nth 1
       #+sbcl *posix-argv*
       #+clisp ext:*args*
       #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
       ))

;(test-host "nosuchhost")
(test-host *host*)
(test-file *host*)

(format t "Refs:  ~A~%" (get-refs))
(format t "Bytes: ~A~%" (get-bytes))
