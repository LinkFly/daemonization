(defpackage daemon-share 
  (:use :cl :daemon-features :daemon-logging)
  (:export #:define-constant #:*process-type* #:*fn-exit* 
	   #:ex-ok #:ex-general #:ex-software #:ex-unavailable #:ex-cantcreate
	   #:+pid-file-not-found+ #:+pid-file-exists+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path
	   #:call-file-exists-error #:file-exists-error
	   ;;; logging
	   #:log-info #:log-err #:defun-ext #:wrap-log
	   #:*print-log-info* #:*print-log-err*
	   #:*log-indent* #:*print-log-layer* #:*print-internal-call* 
	   #:*print-call #:*print-called-form-with-result*
	   #:*print-pid*
	   #:*fn-log-info* #:*fn-log-err* #:*log-prefix*
	   #:add-daemon-log #:get-daemon-log-list
	   #:*print-log-datetime* #:*fn-log-pid*
	   ))

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
(defconstant ex-cantcreate 73)
(defconstant ex-unavailable 69)

(defconstant +pid-file-not-found+ 256)
(defconstant +pid-file-exists+ 257)
 
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

(define-condition file-exists-error (error) ((pathname :initarg :pathname :accessor file-exists-error-pathname)))

(defun call-file-exists-error (pathname)
  (error (make-condition 'file-exists-error
			 :pathname pathname
			 :format-control "File already exists: ~S"
			 :format-arguments (list pathname))))


