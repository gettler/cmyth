;;;; exception.lisp

(in-package #:cmyth)

(define-condition exception (error)
  ((text :initarg :text :reader text)))
