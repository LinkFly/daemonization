(defpackage daemon-share 
  (:use :cl :daemon-features :daemon-logging)
  (:export #:define-constant 
	   #:get-real-file
	   #:*process-type* ;;Must be nil or :parent or :child. Needed for daemonize (there reading) and fork (there set)")
	   #:*fn-exit*
	   #:+ex-ok+ #:+ex-general+ #:+ex-software+ #:+ex-unavailable+ #:+ex-cantcreate+
	   #:+pid-file-not-found+ #:+pid-file-exists+ #:+process-not-exists+
	   #:+system-name+ #:get-system-path #:absolute-path-p #:ensure-absolute-path
	   #:call-file-exists-error #:file-exists-error 
	   #:call-passwd-struct-not-found-error #:passwd-struct-not-found-error
	   #:call-group-struct-not-found-error #:group-struct-not-found-error
	   #:call-group-change-but-user-not-change-error #:group-change-but-user-not-change-error
	   #:call-timeout-forked-process-response-error #:timeout-forked-process-response-error
	   #:call-bad-interface-error #:bad-interface-error
	   #:call-bad-start-pathname-error #:bad-start-pathname-error
	   #:call-denied-change-user-error #:denied-change-user-error
	   #:pathname-as-directory 
	   #:*timeout-daemon-response*	   
	   #:*listen-privileged-ports*

	   #:plist-to-logger

	   ;;; Logging
	   #:*logger*
	   #:with-tmp-logger
	   #:print-call-p #:print-pid-p
	   #:log-info #:log-err #:defun-ext #:wrap-log
	   #:print-log-info-p #:print-log-info-load-p #:print-log-err-p
	   #:*log-indent* #:print-log-layer-p #:print-internal-call-p
	   #:print-called-form-with-result-p
	   
	   #:fn-log-info #:fn-log-info-load #:fn-log-err #:fn-log-trace #:*log-prefix*
	   #:add-daemon-log #:get-daemon-log-list
	   #:print-log-datetime-p
	   #:disabled-functions-logging
	   #:disabled-layers-logging	   
	   #:*syslog-cleaning-p*
	   #:*stopping-max-secs*
	   #:*log-line-number*
	   #:*print-log-line-number*
	   #:*fn-correct-log-plist*
	   #:*main-function-symbol*
	   #:print-username-p #:print-groupname-p
	   #:fn-get-pid #:fn-get-username #:fn-get-groupname
	   #:fn-get-datetime

	   ;; for finding pid-files 
	   #:*pid-files-dirname* #:get-pid-files-dir 

	   ;; for finding conf-files 
	   #:*conf-files-dirname* #:get-conf-files-dir #:*default-conf-file-name* #:get-default-conf-file 
	   #:*conf-log-file* #:get-logging-conf-file

	   ;; for setting log files	   
	   #:*log-file-dir* #:get-log-file-dir	   

	   ;; utils
	   #:with-keys

	   ;; for checking parameters and result
	   #:+conf-parameters+
	   #:+result-keys+
	   #:status
	   #:plist
	   #:config-plist
	   #:result-plist
	   
	   ;; for debugging
	   #:*backtrace-count*	   

	   ;;; Struct functions
	   #:extra-status
	   #:make-extra-status
	   #:extra-status-p
	   #:copy-extra-status
	   #:extra-status-pid
	   #:extra-status-exit-code
	   #:extra-status-name
	   #:extra-status-pid-file
	   #:extra-status-user

	   #:logger
	   #:make-logger
	   #:logger-p
	   #:copy-logger
	   #:logger-files-dir
	   #:logger-admin-files-dir
	   #:logger-info-destination
	   #:logger-error-destination
	   #:logger-trace-destination
	   #:logger-admin-info-destination
	   #:logger-admin-error-destination
	   #:logger-admin-trace-destination

	   #:error-description
	   #:make-error-description
	   #:error-description-p
	   #:copy-error-description
	   #:error-description-condition
	   #:error-description-source
	   #:error-description-place
	   #:error-description-backtrace	   
	   #:error-description-source-more
	   #:error-description-corrupted-form
	   #:logger-count
	   #:log-info-load))

(in-package :daemon-share)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +system-name+ :daemonization)

(defparameter *syslog-cleaning-p* t "Removing from output to syslog string #012 and spaces")
(defparameter *stopping-max-secs* 60 "Maximum time for tries normal stopping daemons")
(defparameter *pid-files-dirname* "pid-files" "Default directory for saving pid-files")
(defparameter *conf-files-dirname* "conf-files" "Default directory for config files")
(defparameter *default-conf-file-name* "default.conf" "From this file do reading all are not setting parameters")
(defparameter *conf-log-file* "default-logging.conf" "Parameters for logging")

(defparameter *listen-privileged-ports* t "If t enabled feature listening privileged (system) ports")
(defparameter *main-function-symbol* nil "Setting in :daemonization package. Needed for correct reset
:line property (in log-plist) in function call *fn-correct-log-plist*")

(defstruct (logger (:include base-logger))
  (files-dir "logs" :type string)
  (admin-files-dir "logs" :type string)
  (info-destination :system :type (or string pathname (eql :system)))
  (error-destination :system :type (or string pathname (eql :system)))
  (trace-destination :system :type (or string pathname (eql :system)))
  (admin-info-destination :system :type (or string pathname (eql :system)))
  (admin-error-destination :system :type (or string pathname (eql :system)))
  (admin-trace-destination :system :type (or string pathname (eql :system)))
  (count '(1 1))

  fn-log-info-load ;;Function for logging at load time
  fn-get-pid 
  fn-get-username
  fn-get-groupname

  (print-pid-p t)
  (print-username-p t)
  (print-groupname-p t)
  
  (print-log-info-load-p t)
  )

(defparameter *timeout-daemon-response* 5 "Second for waiting init daemon(child process 
after fork). If daemon not response - calling timeout-forked-process-response-error")

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

(deftype status ()
  `(member ,@(list 
	      +ex-ok+
	      +ex-general+ 
	      +ex-software+ 
	      +ex-cantcreate+ 
	      +ex-unavailable+ 
	      +pid-file-not-found+
	      +pid-file-exists+ 
	      +process-not-exists+)))

(define-constant +conf-parameters+ '(:before-init-fn :main-function :name :user :group :pid-file :pid-file-dir
				     :before-parent-exit-fn :exit :os-params :parent-conf-file :parent-conf-file-dir
				     :listen-privileged-ports))
(define-constant +result-keys+ '(:result :command :status :reason :pid :pid-file :internal-result)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
(defmacro with-keys (keys plist &body body)
  (let ((pls (gensym "PLIST-")))
    `(let ((,pls ,plist))
       (symbol-macrolet ,(loop for key in keys
			    collect `(,key (getf ,pls ,(intern (symbol-name key) :keyword))))	   
	 ,@body))))

(defun plist-p (value)
  (and (consp value)
       (evenp (length value))
       (loop for key-and-rest on value by #'cddr
	  always (keywordp (first key-and-rest)))))

(deftype plist ()
  `(satisfies plist-p))

(defun get-keys-of-plist (plist)
  (loop for key-and-rest on plist by #'cddr
     collect (first key-and-rest)))

(defun unique-elements-p (list)
  (= (length list) (length (remove-duplicates list))))
;(unique-elements-p '(:conf-params nil))
(defun config-plist-p (plist &aux (keys (get-keys-of-plist plist)))
  (cond 
    ((not (unique-elements-p +conf-parameters+)) (error "Not unique keys in +conf-parameters+"))
    ((not (unique-elements-p keys)) (error "Not unique keys in keys of plist"))
    (t (subsetp keys +conf-parameters+))))

(deftype config-plist ()
  `(and plist
	(satisfies config-plist-p)))

(defun result-plist-p (plist &aux (keys (get-keys-of-plist plist)))
  (cond 
    ((not (unique-elements-p +result-keys+)) (error "Not unique keys in +result-keys+"))
    ((not (unique-elements-p keys)) (error "Not unique keys in keys of result-plist"))
    (t (and (plist-p plist) (subsetp keys +result-keys+)))))

(deftype result-plist ()
  `(satisfies result-plist-p))

(defun plist-to-logger (plist)
  (loop with logger = (make-logger)
     for cur-plist on plist by #'cddr
     for key = (first cur-plist)
     for slot = (find-symbol (symbol-name key) (symbol-package 'logger))
     if (slot-exists-p logger slot)
      do (setf (slot-value logger slot) (second cur-plist))
     else do (error "Slot ~S not exist. Bad plist - not appropriate of the logger struct." slot)
     finally (return logger)))

;;;;;;;;;;;;;;;;;;;;;; end utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;; For debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *backtrace-count* 20)

(defstruct (error-description (:print-object (lambda (obj stream)
					       (format stream "~%")
					       (loop for slot in '(condition source corrupted-form
								   place backtrace source-more)
						  do (format stream (substitute #\# #\~ (format nil "~A: ~S~%" slot (slot-value obj slot)))))
					       )))
  condition source corrupted-form place backtrace source-more)

;(progn (princ (make-error-description)) (values))
;(defun err (format t "~%ERROR DESCRIPTION:~%~S~%" (f2))
;;;;;;;;;;;;;;;;;;;;; end for debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pathnames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun get-real-file (file &optional dir-or-fn-get-dir)
  (declare (type (or pathname string function null) dir-or-fn-get-dir)
	   (type (or pathname string null) file))
  (when (null file) (return-from get-real-file))
  (if (absolute-path-p file)
      file
      (let ((pathname (typecase dir-or-fn-get-dir
				 ((or pathname string) dir-or-fn-get-dir)
				 (function (funcall dir-or-fn-get-dir))
				 (null (get-system-path)))))
	(make-pathname :defaults pathname :name (pathname-name file) :type (pathname-type file)))))

(defun get-pid-files-dir ()
  (when (absolute-path-p *pid-files-dirname*) 
    (return-from get-pid-files-dir (pathname-as-directory *pid-files-dirname*)))
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list *pid-files-dirname*))))

(defun get-conf-files-dir ()
  (when (absolute-path-p *conf-files-dirname*) 
    (return-from get-conf-files-dir (pathname-as-directory *conf-files-dirname*)))
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list *conf-files-dirname*))))

(defun get-default-conf-file ()
  (when (absolute-path-p *default-conf-file-name*) 
    (return-from get-default-conf-file *default-conf-file-name*))
  (make-pathname :defaults (get-conf-files-dir)
		 :name *default-conf-file-name*))

(defun get-logging-conf-file ()
  (when (absolute-path-p *conf-log-file*) 
    (return-from get-logging-conf-file *conf-log-file*))
  (make-pathname :defaults (get-conf-files-dir)
		 :name *conf-log-file*))

;;;;;;;;;;;;;;;;;;;;;;;; end pathnames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;; logging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro log-info-load (log-str &rest args) 
  `(when (logger-print-log-info-load-p *logger*)
     (with-tmp-logger ((fn-log-info (slot-value *logger* 'fn-log-info-load)))
       (log-info ,log-str ,@args)
       )))

;;;;;;;;;; Conditions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-condition file-exists-error () 
  ((pathname :initarg :pathname :accessor file-exists-error-pathname))
  (:report (lambda (condition stream)
	     (format stream "File already exists: ~S" (file-exists-error-pathname condition)))))

(define-condition passwd-struct-not-found-error () 
  ((user-name :initarg :user-name :accessor user-name))
  (:report (lambda (condition stream)
	     (format stream "Not found passwd structure. User name = ~S. Bad user name?"
		     (user-name condition)))))

(define-condition group-struct-not-found-error () 
  ((group-name :initarg :group-name :accessor group-name))
  (:report (lambda (condition stream)
	     (format stream "Not found group structure. Group name = ~S. Bad group name?" 
		     (group-name condition)))))

(define-condition timeout-forked-process-response-error ()
  ((timeout :initarg :timeout :accessor timeout))
  (:report (lambda (condition stream)
	     (format stream "Created daemon not response. Very small *timeout-daemon-response*(~A) or daemon init error(not running)?"
		     (timeout condition)))))

(define-condition group-change-but-user-not-change-error ()
  ((group-name :initarg :group-name :accessor group-name))
  (:report (lambda (condition stream)
	     (format stream "Can't change the group, but not modify user. Group name: ~S"
		     (group-name condition)))))

(define-condition bad-interface-error ()
  ((fn-name :initarg :fn-name :accessor fn-name)
   (args-lambda-list :initarg :args-lambda-list :accessor args-lambda-list)
   (target-package :initarg :target-package :accessor target-package)
   (source-package :initarg :source-package :accessor source-package))
  (:report (lambda (condition stream)
	     (with-slots (fn-name args-lambda-list target-package source-package) condition
	       (format stream 
		       "Bad interface. Function name: ~A. Lambda-list: ~A. Target package: ~A. Source package: ~A"
		       fn-name args-lambda-list (package-name target-package) (package-name source-package))))))

(define-condition bad-start-pathname-error ()
  ((source/load-pathname :initarg :source/load-pathname :accessor source/load-pathname))
  (:report (lambda (condition stream)
	     (format stream "Bad source/load pathname. Character <Tilde> in the way. Pathname: ~A"
		     (source/load-pathname condition)))))

(define-condition denied-change-user-error ()
  ((cur-user :initarg :cur-user :accessor cur-user)
   (new-user :initarg :new-user :accessor new-user))
  (:report (lambda (condition stream)
	     (format stream "Denied changing user when current user is not administrator. Current user: ~A. New user: ~A"
		     (cur-user condition)
		     (new-user condition)))))

(defun call-denied-change-user-error (cur-user new-user)
  (error 'denied-change-user-error :cur-user cur-user :new-user new-user))

(defun call-bad-start-pathname-error (source/load-pathname)
  (error 'bad-start-pathname-error
	 :source/load-pathname source/load-pathname))

(defun call-file-exists-error (pathname)
  (error 'file-exists-error
	 :pathname pathname))

(defun call-passwd-struct-not-found-error (user-name)
  (error 'passwd-struct-not-found-error
	 :user-name user-name))

(defun call-group-struct-not-found-error (group-name)
  (error 'group-struct-not-found-error
	 :group-name group-name))

(defun call-timeout-forked-process-response-error (timeout)
  (error 'timeout-forked-process-response-error
	 :timeout timeout))

(defun call-group-change-but-user-not-change-error (group-name)
  (error 'group-change-but-user-not-change-error
	 :group-name group-name))

(defun call-bad-interface-error (fn-name args-lambda-list target-package source-package)
  (error 'bad-interface-error
	 :fn-name fn-name
	 :args-lambda-list args-lambda-list
	 :target-package target-package
	 :source-package source-package))
;;;;;;;;;;;;;; end conditions ;;;;;;;;;;;;;;;;;;


