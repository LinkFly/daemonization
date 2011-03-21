(defpackage share 
  (:use :cl)
  (:export #:define-constant #:*process-type*))

(in-package :share)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defparameter *process-type* nil
  "Must be nil or :parent or :child. Needed for daemonize (there reading) and fork (there set)")