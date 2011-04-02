(defpackage :daemonization-test 
  (:use :cl :daemonization)
  (:export #:run-tests #:root-run-tests #:getpid))

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

(defun get-full-pathname (log-file-name)
  (make-pathname :defaults (get-test-dir)
		 :type nil
		 :name log-file-name))

(defun get-log-file ()
  (case *test-mode*
    (:user (get-full-pathname *log-filename*))
    (:root (get-full-pathname *root-log-filename*))))

(defun get-syslog-file (&aux res)
  (case *test-mode*
    (:user (get-full-pathname *syslog-filename*))
    (:root (get-full-pathname *root-syslog-filename*))))
		 
(defun daemon-cmd (cmd)
  (apply #'daemonization:daemonized	 
	 (case *test-mode*
	   (:user (list *daemon-conf* cmd 
			:on-error :as-ignore-errors))
	   (:root (list *root-daemon-conf* cmd 
			:print-extra-status t 
			:on-error :as-ignore-errors)))))

(defun daemon-status ()
  (daemon-cmd "status"))

(defun ensure-no-syslog-file ()
  (when (probe-file (get-syslog-file))
    (delete-file (get-syslog-file))))

(defun recreate-root-syslog-file ()
  (let ((*test-mode* :root)) 
;;test
;    (sb-posix:umask 0)
;;end test
    (daemonization:recreate-file-allow-write-other (get-syslog-file))))

(defun print-syslog (fmt-str &rest args)
  (with-open-file (syslog-stream (get-syslog-file)
				 :direction :output :if-does-not-exist :create :if-exists :append)
    (apply #'format syslog-stream fmt-str args)))

(defun open-broadcast-log-stream ()
  (make-broadcast-stream
   (open (get-log-file) :direction :output :if-does-not-exist :create :if-exists :supersede)
   *standard-output*))

(defun run-tests () 			 
  (let* ((parent-pid (daemonization:getpid))
	 (*test-mode* :user)
	 (*standard-output* (open-broadcast-log-stream))
	 (daemonization:*fn-log-info* #'print-syslog)
	 (daemonization:*fn-log-err* #'print-syslog))
    (ensure-no-syslog-file)
    (princ "Tests daemonized ... ")
    (terpri)
    (block tests 
      (flet ((return-if-child () 
	       (when (/= parent-pid (daemonization:getpid))
		 (return-from tests t))))
	(if (and 
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try kill ...~%") (daemon-cmd "kill") (not (eql daemon-share:ex-ok (daemon-status))))
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try restart ...~%") (daemon-cmd "restart") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try stop ...~%")(daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	     )
	    (format t "~% ... Tests passed.")
	    (format t "~% ... Tests failed.")))
      (terpri)
      (finish-output *standard-output*)
					;(close *broadcast-stream*)
					;(close *log-prompts-stream*)
      )))

(defun root-run-tests (username)
  (let* ((parent-pid (daemonization:getpid))
	 (*test-mode* :root)
	 (*standard-output* (open-broadcast-log-stream))
	 (daemonization:*fn-log-info* #'print-syslog)
	 (daemonization:*fn-log-err* #'print-syslog))
    (ensure-no-syslog-file)
    (recreate-root-syslog-file)
;;test
#|
    (sb-posix:umask 0)
    (delete-file #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/tests/test-file")
    (daemonization:create-file-allow-write-other #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/tests/test-file")
    (sb-posix:umask 0)
|#
;;end test
    
    (princ "Tests daemonized (with is changing of user) ... ")
    (terpri)
    (block tests 
      (flet ((return-if-child () 
	       (when (/= parent-pid (daemonization:getpid))
		 (return-from tests t))))
	(if (and 	   
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try kill ...~%") (daemon-cmd "kill") (not (eql daemon-share:ex-ok (daemon-status))))
	     (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try restart ...~%") (daemon-cmd "restart") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	     (progn (format t "~%try stop ...~%")(daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	      
	     (let ((*root-daemon-conf* *root-daemon-conf*))
	       (setf (getf *root-daemon-conf* :user) username)
	       (format t "~%--- Try is changing of user ---~%")
	       (and
		(progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) 
		       (equal (list daemon-share:ex-ok username)
			      (multiple-value-bind (status extra-status)
				  (daemon-status)
				(list status (getf extra-status :user)))))
		(progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))))
	     ) ;and
	    (format t "~% ... Tests passed.")
	    (format t "~% ... Tests failed."))
	(terpri)	
	(finish-output *standard-output*)
					;(close *broadcast-stream*)
					;(close *log-prompts-stream*)
	))))