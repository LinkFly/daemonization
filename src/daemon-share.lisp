(defpackage daemon-share 
  (:use :cl)
  (:export #:define-constant #:*process-type* #:*fn-exit* #:ex-ok #:ex-software #:ex-unavailable
	   #:+pid-file-not-found+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path))

(in-package :daemon-share)

(define-constant +system-name+ :daemonization)
(defun get-system-path ()
  (make-pathname :defaults
		 (asdf:component-pathname 
		  (asdf:find-system +system-name+))
		 :name nil :type nil))

(defun absolute-path-p (pathname)
  (eq :absolute (first (pathname-directory pathname))))

(defun ensure-absolute-path (path)
  (if (not (absolute-path-p path))
      (make-pathname :defaults (get-system-path)
		     :name path)))

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defparameter *process-type* nil
  "Must be nil or :parent or :child. Needed for daemonize (there reading) and fork (there set)")

(defparameter *fn-exit* nil)

(defconstant ex-ok 0)
(defconstant ex-software 70)
(defconstant ex-unavailable 69)
(defconstant +pid-file-not-found+ 256)