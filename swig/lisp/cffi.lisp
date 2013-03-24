;;;;
;;;; Copyright (C) 2013, Jon Gettler
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the Lisp Lesser General Public License version 2,
;;;; as published by the Free Software Foundation and with the following
;;;; preamble: http://opensource.franz.com/preamble.html
;;;;

(in-package #:cmyth)

(cffi:defcfun ("ctime" c-ctime) :string (tm :pointer))

(defconstant EINTR 4)
