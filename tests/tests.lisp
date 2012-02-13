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

(defun ensure-no-pid-file ()
  (ensure-no-file (get-pid-file)))

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
 
(defun run-tests-share (user/root start-message &optional fn-extra-tests)
  (with-error-handling
    (let* ((*test-mode* user/root)
	   (parent-pid (get-proc-id))
	   (fn-child-proc-p (lambda () (/= parent-pid (get-proc-id))))
	   (prompts-file-stream (open (get-log-file)
				      :direction :output
				      :if-does-not-exist :create
				      :if-exists :supersede))
	   (*standard-output* (make-broadcast-stream prompts-file-stream *standard-output*)))
      (princ start-message)
      (terpri)
      (unwind-protect
	   (block tests 
	     (flet ((return-if-child () 
		      (when (funcall fn-child-proc-p)
			(return-from tests t)))
		    (daemon-status () (getf (getf (daemon-status) :internal-result) :status)))
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
			(if fn-extra-tests 
			    (funcall fn-extra-tests #'return-if-child)
			    t)
			)
		     (error (condition) (format t "~%ERROR: ~A~%" condition)))
		   (format t "~% ... Tests passed.")
		   (format t "~% ... Tests failed."))

	       (terpri)	
	       (finish-output *standard-output*)
	       (close *standard-output*)
	       (close prompts-file-stream)
	       ))
	(unless (funcall fn-child-proc-p) (ensure-no-pid-file))))))

(defun root-extra-tests (username fn-return-if-child)
  (flet ((daemon-status-internal-result () (getf (daemon-status) :internal-result)))
    (let ((*root-daemon-conf* *root-daemon-conf*))
      (setf (getf *root-daemon-conf* :user) username)
      (format t "~%--- Try is changing of user ---~%")
      (and
       (progn (format t "~%try start ...~%") (daemon-cmd "start") (funcall fn-return-if-child) 	      
	      (equal (list daemon-share:+ex-ok+ username)
		     (let* ((status-second (daemon-status-internal-result))
			    (status (getf status-second :status))
			    (extra-status (getf status-second :extra-status)))
		       (list status (extra-status-user extra-status)))))
       (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:+ex-ok+ (daemon-status))))))))

(defun run-tests ()
  (run-tests-share :user "Tests daemonized ... "))

(defun root-run-tests (username)
  (run-tests-share :root
		   "Tests daemonized (with changing user) ... "
		   (lambda (fn-return-if-child) (root-extra-tests username fn-return-if-child))))
