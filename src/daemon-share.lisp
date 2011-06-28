(defpackage daemon-share 
  (:use :cl :daemon-features :daemon-logging)
  (:export #:define-constant #:*process-type* #:*fn-exit* 
	   #:+ex-ok+ #:+ex-general+ #:+ex-software+ #:+ex-unavailable+ #:+ex-cantcreate+
	   #:+pid-file-not-found+ #:+pid-file-exists+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path
	   #:call-file-exists-error #:file-exists-error #:absolute-path-p

	   ;;; Struct functions
	   #:MAKE-EXTRA-STATUS #:EXTRA-STATUS-EXIT-CODE #:EXTRA-STATUS-PID 
	   #:EXTRA-STATUS-NAME #:EXTRA-STATUS-PID-FILE #:EXTRA-STATUS-USER
	   #:NAME #:PID-FILE

	   ;;; Logging
	   #:log-info #:log-err #:defun-ext #:wrap-log
	   #:*print-log-info* #:*print-log-err*
	   #:*log-indent* #:*print-log-layer* #:*print-internal-call* 
	   #:*print-call #:*print-called-form-with-result*
	   #:*print-pid*
	   #:*fn-log-info* #:*fn-log-err* #:*fn-log-trace* #:*log-prefix*
	   #:add-daemon-log #:get-daemon-log-list
	   #:*print-log-datetime* #:*fn-log-pid*
	   #:*disabled-functions-logging*
	   #:*disabled-layers-logging*

	   ;; for finding pid-files 
	   #:*pid-files-dirname* #:get-pid-files-dir))

(in-package :daemon-share)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +system-name+ :daemonization)
(defparameter *process-type* nil
  "Must be nil or :parent or :child. Needed for daemonize (there reading) and fork (there set)")

(defparameter *pid-files-dirname* "pid-files" "Default directory for saving pid-files")

(defstruct extra-status 
  pid exit-code name pid-file user)
(defparameter *fn-exit* nil "Function for none local exit. Must be have parameters (&optional (status +ex-ok+) extra-status).
Return value must be status value or list contained status value and value type of extra-status.")

(defconstant +ex-ok+ 0)
(defconstant +ex-general+ 1)
(defconstant +ex-software+ 70)
(defconstant +ex-cantcreate+ 73)
(defconstant +ex-unavailable+ 69)

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
  (if (not (absolute-path-p path))
      (make-pathname :defaults (get-system-path)
		     :name path)
      path))

(defun get-pid-files-dir ()
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list *pid-files-dirname*))))

(define-condition file-exists-error (error) ((pathname :initarg :pathname :accessor file-exists-error-pathname)))

(defun call-file-exists-error (pathname)
  (error (make-condition 'file-exists-error
			 :pathname pathname
			 :format-control "File already exists: ~S"
			 :format-arguments (list pathname))))

