(defpackage :daemonization-test 
  (:use :cl :daemonization :daemonization-utils)
  (:export #:run-tests #:root-run-tests #:get-proc-id))

(in-package :daemonization-test)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defconstant +system-name+ :daemonization-test)
(define-constant +test-dir+ "tests")

(defparameter *test-mode* nil "Maybe :user or :root")

(defparameter *log-filename* "tests-prompts.log")
(defparameter *root-log-filename* "root-tests-prompts.log")
(defparameter *syslog-filename* "tests-syslog.log")
(defparameter *root-syslog-filename* "root-tests-syslog.log")

(defparameter *daemon-conf* '(:main-function  nil
			      :pid-file "daemon-test-pid"
			      :exit nil
			      :name nil
			      :user nil
			      :group nil
			      :before-parent-exit-fn nil))

(defparameter *root-daemon-conf* '(:main-function  nil
				   :pid-file "root-daemon-test-pid"
				   :exit nil
				   :name nil
				   :user nil
				   :group nil
				   :before-parent-exit-fn nil))

(defun get-system-path ()
  (asdf:component-pathname (asdf:find-system +system-name+)))

(defun get-test-dir ()
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list +test-dir+))))

(defun get-full-pathname (file-name)
  (make-pathname :defaults (get-test-dir)
		 :type nil
		 :name file-name))

(defun get-log-file ()
  (case *test-mode*
    (:user (get-full-pathname *log-filename*))
    (:root (get-full-pathname *root-log-filename*))))

(defun get-syslog-file ()
  (case *test-mode*
    (:user (get-full-pathname *syslog-filename*))
    (:root (get-full-pathname *root-syslog-filename*))))

(defun get-pid-file ()
  ((lambda (conf) 
     (make-pathname :defaults (get-pid-files-dir) :name (getf conf :pid-file)))
   (case *test-mode*
     (:user *daemon-conf*)
     (:root *root-daemon-conf*))))

(defun daemon-cmd (cmd)
  (apply #'daemonization:daemonized	 
	 (case *test-mode*
	   (:user (list *daemon-conf*
			cmd
			:print-result-type :simple
			:on-error :as-ignore-errors 
			:recreate-pid-file-on-start t))
	   (:root (list *root-daemon-conf*
			cmd
			:print-result-type :simple
			:print-internal-result t
			:on-error :as-ignore-errors 
			:recreate-pid-file-on-start t)))))

(defun daemon-status ()
  (daemon-cmd "status"))

(defun ensure-no-file (pathname)
  (when (probe-file pathname)    
    (delete-file pathname)))

(defun ensure-no-syslog-file ()
  (ensure-no-file (get-syslog-file)))

(defun ensure-no-pid-file ()
  (ensure-no-file (get-pid-file)))

(defun recreate-root-syslog-file ()
  (let ((*test-mode* :root)) 
    (recreate-file-allow-write-other (get-syslog-file))))

(defun print-syslog (fmt-str &rest args)
  (with-open-file (syslog-stream (get-syslog-file)
				 :direction :output :if-does-not-exist :create :if-exists :append)
    (apply #'format syslog-stream fmt-str args)))

(defun read-conf-params (file)
  (with-open-file (stream file)
    (read stream)))

(defun test-config-handling ()
  (let ((base-conf "base-conf-params.conf")
	(result-conf "result-conf-params.conf")
	(*conf-files-dirname* "tests")
	(*default-conf-file-name* (make-pathname :defaults (get-conf-files-dir) :name *default-conf-file-name*)))
    (equal-conf-params 
     (probe-conf-params (read-conf-params (get-full-pathname base-conf)))
     (read-conf-params (get-full-pathname result-conf)))))
;(test-config-handling)

(defmacro with-error-handling (&body body)
  `(handler-case ,@body
     (error (err) (format t "~%ERROR: Bug in the tests code. Error: ~A~%~% ... Tests failed.~%" err))))
 
(defun run-tests () 			 
  (with-error-handling
    (let* ((parent-pid (get-proc-id))
	   (fn-child-proc-p (lambda () (/= parent-pid (get-proc-id))))
	   (*test-mode* :user)
	   (prompts-file-stream (open (get-log-file)
				      :direction :output
				      :if-does-not-exist :create
				      :if-exists :supersede))
	   (*standard-output* (make-broadcast-stream prompts-file-stream *standard-output*))
	   (fn-wrapped-print-syslog (let ((test-mode-val *test-mode*))
				      #'(lambda (fmt-str &rest args) 
					  (let ((*test-mode* test-mode-val))
					    (apply #'print-syslog fmt-str args)))))
	   (*fn-log-info* fn-wrapped-print-syslog)
	   (*fn-log-err* fn-wrapped-print-syslog)
	   (*fn-log-trace* fn-wrapped-print-syslog))
      (ensure-no-syslog-file)
      (princ "Tests daemonized ... ")
      (terpri)
      (unwind-protect 
	   (block tests 
	     (flet ((return-if-child () 
		      (when (/= parent-pid (get-proc-id))
			(return-from tests t)))
		    (daemon-status () (getf (second (multiple-value-list (daemon-status))) :status)))
	       
	       (if (handler-case 
		       (and 
			(progn (format t "~%try test handling of the configuration ... ") (if (test-config-handling) 
												 (progn (format t "OK~%") t)
												 (format t "FAIL~%")))
			(progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
			(progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))
			(progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
			(progn (format t "~%try kill ...~%") (daemon-cmd "kill") (not (eql daemon-share:+ex-ok+ (daemon-status))))
			(progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
			(progn (format t "~%try restart ...~%") (daemon-cmd "restart") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
			(progn (format t "~%try stop ...~%")(daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))
			)
		     (error (condition) (format t "~%ERROR: ~A~%" condition)))
		   (format t "~% ... Tests passed.")
		   (format t "~% ... Tests failed."))
	       (terpri)
	       (finish-output *standard-output*)
	       (close *standard-output*) 
	       (close prompts-file-stream)
	       ))			;block tests
	(unless (funcall fn-child-proc-p) (ensure-no-pid-file))))))

(defun root-run-tests (username)
  (with-error-handling
    (let* ((parent-pid (get-proc-id))
	   (fn-child-proc-p (lambda () (/= parent-pid (get-proc-id))))
	   (*test-mode* :root)
	   (prompts-file-stream (open (get-log-file)
				      :direction :output
				      :if-does-not-exist :create
				      :if-exists :supersede))
	   (*standard-output* (make-broadcast-stream prompts-file-stream *standard-output*))
	   (fn-wrapped-print-syslog (let ((test-mode-val *test-mode*))
				      #'(lambda (fmt-str &rest args) 
					  (let ((*test-mode* test-mode-val))
					    (apply #'print-syslog fmt-str args)))))
	   (*fn-log-info* fn-wrapped-print-syslog)
	   (*fn-log-err* fn-wrapped-print-syslog)
	   (*fn-log-trace* fn-wrapped-print-syslog))
      (ensure-no-syslog-file)
      (recreate-root-syslog-file)    
      (princ "Tests daemonized (with is changing of user) ... ")
      (terpri)
      (unwind-protect
	   (block tests 
	     (flet ((return-if-child () 
		      (when (funcall fn-child-proc-p)
			(return-from tests t)))
		    (daemon-status () (getf (second (multiple-value-list (daemon-status))) :status))
		    (daemon-status-second-value () (second (multiple-value-list (daemon-status)))))

	       (if (and 
		    (progn (format t "~%try test handling of the configuration ... ") (if (test-config-handling) 
											     (progn (format t "OK~%") t)
											     (format t "FAIL~%")))		    
		    (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
		    (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))
		    (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
		    (progn (format t "~%try kill ...~%") (daemon-cmd "kill") (not (eql daemon-share:+ex-ok+ (daemon-status))))
		    (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
		    (progn (format t "~%try restart ...~%") (daemon-cmd "restart") (return-if-child) (eql daemon-share:+ex-ok+ (daemon-status)))
		    (progn (format t "~%try stop ...~%")(daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))
	      
		    (let ((*root-daemon-conf* *root-daemon-conf*))
		      (setf (getf *root-daemon-conf* :user) username)
		      (format t "~%--- Try is changing of user ---~%")
		      (and
		       (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) 
			      (equal (list daemon-share:+ex-ok+ username)
				     (let* ((status-second (daemon-status-second-value))
					    (status (getf status-second :status))
					    (extra-status (getf status-second :extra-status)))
				       (list status (extra-status-user extra-status)))))
		       (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))))
		    )			;and
		   (format t "~% ... Tests passed.")
		   (format t "~% ... Tests failed."))
	  
	       (terpri)	
	       (finish-output *standard-output*)
	       (close *standard-output*)
	       (close prompts-file-stream)
	       ))			;block tests
	(unless (funcall fn-child-proc-p) (ensure-no-pid-file))))))