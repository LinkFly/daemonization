(defpackage daemon-share 
  (:use :cl)
  (:export #:define-constant #:*process-type* #:*fn-exit* #:ex-ok #:ex-software #:ex-unavailable
	   #:+pid-file-not-found+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path #:ex-general))

(in-package :daemon-share)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +system-name+ :daemonization)
(defparameter *process-type* nil
  "Must be nil or :parent or :child. Needed for daemonize (there reading) and fork (there set)")

(defparameter *fn-exit* nil "Function for none local exit. Must be have parameters (&optional (status ex-ok) &rest extra-status).")
;(function-lambda-expression *fn-exit*)

(defconstant ex-ok 0)
(defconstant ex-general 1)
(defconstant ex-software 70)
(defconstant ex-unavailable 69)
(defconstant +pid-file-not-found+ 256)
 
(defun get-system-path ()
  (make-pathname :defaults
		 (asdf:component-pathname 
		  (asdf:find-system +system-name+))
		 :name nil :type nil))

(defun absolute-path-p (pathname)
  (eq :absolute (first (pathname-directory pathname))))

(defun ensure-absolute-path (path)
;(setq path #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/my-daemon-exp")
  (if (not (absolute-path-p path))
      (make-pathname :defaults (get-system-path)
		     :name path)
      path))
