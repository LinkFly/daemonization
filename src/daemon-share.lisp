(defpackage daemon-share 
  (:use :cl :daemon-features :daemon-logging)
  (:export #:define-constant #:*process-type* #:*fn-exit*
	   #:+ex-ok+ #:+ex-general+ #:+ex-software+ #:+ex-unavailable+ #:+ex-cantcreate+
	   #:+pid-file-not-found+ #:+pid-file-exists+ #:+process-not-exists+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path
	   #:call-file-exists-error #:file-exists-error #:absolute-path-p
	   #:call-passwd-struct-not-found-error #:call-group-struct-not-found-error
	   #:call-group-change-but-user-not-change-error
	   #:pathname-as-directory 

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
(defconstant +process-not-exists+ 258)
 
(defun get-system-path ()
  (asdf:component-pathname 
   (asdf:find-system +system-name+)))

(defun absolute-path-p (pathname)
  (eq :absolute (first (pathname-directory pathname))))

(defun ensure-absolute-path (path)
  (if (not (absolute-path-p path))
      (make-pathname :defaults (get-system-path)
		     :name path)
      path))

(defun component-present-p (value)
  "Helper function for DIRECTORY-PATHNAME-P which checks whether VALUE
is neither NIL nor the keyword :UNSPECIFIC."
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC \(a pathname designator) does not designate
a directory, PATHSPEC otherwise.  It is irrelevant whether file or
directory designated by PATHSPEC does actually exist."
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  "Converts the non-wild pathname designator PATHSPEC to directory
form."
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun get-pid-files-dir ()
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list *pid-files-dirname*))))

(define-condition file-exists-error (error) 
  ((pathname :initarg :pathname :accessor file-exists-error-pathname))
  (:report (lambda (condition stream)
	     (format stream "File already exists: ~S" (file-exists-error-pathname condition)))))

(define-condition passwd-struct-not-found-error (error) 
  ((user-name :initarg :user-name :accessor user-name))
  (:report (lambda (condition stream)
	     (format stream "Not found passwd structure. User name = ~S. Bad user name?" (user-name condition)))))

(define-condition group-struct-not-found-error (error) 
  ((group-name :initarg :group-name :accessor group-name))
  (:report (lambda (condition stream)
	     (format stream "Not found group structure. Group name = ~S. Bad group name?" (group-name condition)))))

(define-condition group-change-but-user-not-change-error (error)
  ((group-name :initarg :group-name :accessor group-name))
  (:report (lambda (condition stream)
	     (format stream "Can't change the group, but not modify user. Group name: ~S"
		     (group-name condition)))))

(defun call-file-exists-error (pathname)
  (error 'file-exists-error
	 :pathname pathname))

(defun call-passwd-struct-not-found-error (user-name)
  (error 'passwd-struct-not-found-error
	 :user-name user-name))

(defun call-group-struct-not-found-error (group-name)
  (error 'group-struct-not-found-error
	 :group-name group-name))

(defun call-group-change-but-user-not-change-error (group-name)
  (error 'group-change-but-user-not-change-error
	 :group-name group-name))




