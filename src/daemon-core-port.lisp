(defpackage :daemon-core-port   
  (:use :cl 
	:daemon-share
	:daemon-utils-port
	#-linux (error "Package DAEMON-CORE-PORT not implemented on not Linux")
	#+linux
	:daemon-core-linux-port)
  (:export #:stop-service
	   #:zap-service
	   #:kill-service
	   #:start-service
	   #:simple-start
	   #:status-service

	   #:logging-init
	   #:import-sys-functions-and-constants
	   #:define-unix-functions

	   #:get-error-description))

(in-package :daemon-core-port)

(defun gen-fn-log (type fn-system-log)
  (declare (type (or (eql :info) (eql :error) (eql :trace)) type)
	   (type function fn-system-log))
  (lambda (fmt-str &rest args &aux
	   file-stream-system
	   (is-admin (let ((*print-call* nil)) (admin-current-user-p)))
	   (logger *logger*))
    (flet ((get-file-stream-system (getters-plist)
	     (funcall (funcall (if is-admin #'first #'second)
			       (getf getters-plist type))
		      logger))
	   (get-file-dir (admin-logs-dir-getter logs-dir-getter) 
	     (pathname-as-directory 
	      (get-real-file (funcall (if is-admin admin-logs-dir-getter logs-dir-getter)
				      logger)))))
			     
      (setf file-stream-system (get-file-stream-system
				'(:info (logger-admin-info-destination logger-info-destination)
				  :error (logger-admin-error-destination logger-error-destination)
				  :trace (logger-admin-trace-destination logger-trace-destination))))

      (when (typep file-stream-system '(or string pathname))
	(setf file-stream-system (get-real-file file-stream-system
						(get-file-dir 'logger-admin-files-dir 'logger-files-dir))))
      
      (typecase file-stream-system
	((eql :system) (apply fn-system-log fmt-str args))
	((or pathname string stream) 
	 (apply #'safe-write file-stream-system fmt-str args)
	 (force-output))))))

(defun logging-init ()
  (setf *logger* (plist-to-logger (with-open-file (stream (get-logging-conf-file))
				    (read stream)))
	*fn-log-info* #'(lambda (fmt-str &rest args)
			  (add-daemon-log (apply #'format nil fmt-str args))
			  (apply (gen-fn-log :info #'syslog-info) fmt-str args))
	*fn-log-info-load* nil
	*fn-log-err* #'(lambda (fmt-str &rest args)
			 (add-daemon-log (concatenate 'string "ERROR: " (apply #'format nil fmt-str args)))
			 (apply (gen-fn-log :error #'syslog-err) (concatenate 'string "ERROR: " fmt-str) args))
	*fn-log-trace* #'(lambda (fmt-str)
			   (apply (gen-fn-log :trace #'syslog-info) "~A" (add-daemon-log fmt-str) nil))
	*fn-log-pid* #'(lambda () (let ((*print-call* nil)) (getpid)))
	*fn-correct-log-plist* #'(lambda (log-plist)
				   (when (getf log-plist :line)
				     (labels ((is-log-trace? () 
						(eq :trace (getf log-plist :daemonization)))
					      (is-daemonized-result? ()
						(and (is-log-trace?) 
						     (getf log-plist :result)
						     (eq *main-function-symbol* (getf log-plist :trace-fn)))))
				       (symbol-macrolet ((count-ls (logger-count *logger*)))					 
					 (setf (getf log-plist :line) (copy-list count-ls))
					 (incf (second count-ls))
					 (when (is-daemonized-result?)
					   (setf (first count-ls) (incf (first count-ls)))
					   (setf (second count-ls) 1))
					 log-plist))))))

(declaim (ftype (function (config-plist)) 
		stop-service status-service zap-service kill-service start-service simple-start))
#+daemon.as-daemon
(progn 
  (defun-ext stop-service (params)
    #-linux (error "STOP-SERVICE not implemented on not Linux")
    #+linux
    (stop-daemon (getf params :pid-file)))

  (defun-ext status-service (params)
    #-linux (error "STATUS-SERVICE not implemented on not Linux")
    #+linux
    (status-daemon (getf params :pid-file)))
  
  (defun-ext zap-service (params)
    #-linux (error "ZAP-SERVICE not implemented on not Linux")
    #+linux
    (zap-daemon (getf params :pid-file)))

  (defun-ext kill-service (params)
    #-linux (error "KILL-SERVICE not implemented on not Linux")
    #+linux
    (kill-daemon (getf params :pid-file)))

  (defun-ext start-service (params)
    #-linux (error "START-SERVICE not implemented on not Linux")
    #+linux
    (start-daemon
     (getf params :name)
     (getf params :pid-file)
     :before-parent-exit-fn (getf params :before-parent-exit-fn)
     :configure-rights-fn #'(lambda () 
			      (let ((*listen-privileged-ports* (getf params :listen-privileged-ports)))
				(restrict-rights :new-user (getf params :user) 
						 :new-group (getf params :group))))
     :preparation-fn #'isolate-process
     :before-init-fn (getf params :before-init-fn)
     :main-fn (getf params :main-function)
     :os-params (getf params :os-params)))
  
  ) ;feature :as-daemon					

(defun-ext simple-start (params)
  (let ((*listen-privileged-ports* (getf params :listen-privileged-ports)))
    (restrict-rights :new-user (getf params :user) 
		     :new-group (getf params :group)))
  (start-as-no-daemon (getf params :main-function)))
