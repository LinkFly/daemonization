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

	   #:get-error-description

	   #:syslog-info
	   #:syslog-err))

(in-package :daemon-core-port)

(defun general-success-exit (result-values params)
  (when *fn-exit*
    (funcall *fn-exit*
	     +ex-ok+
	     (make-extra-status :result-values result-values
				:pid (getpid)
				:exit-code +ex-ok+
				:name (getf params :name)
				:pid-file (getf params :pid-file)
				:user (get-username))))
  (exit +ex-ok+))

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
     :main-fn #'(lambda (&aux result-values) 
		  (reinitialized-output-translations)
		  (setf result-values (multiple-value-list (funcall (getf params :main-function))))
		  (general-success-exit result-values params))
     :os-params (getf params :os-params)))
  
  ) ;feature :as-daemon					

(defun-ext simple-start (params)
  (let ((*listen-privileged-ports* (getf params :listen-privileged-ports)))
    (restrict-rights :new-user (getf params :user) 
		     :new-group (getf params :group)))
  (start-as-no-daemon #'(lambda (&aux result-values) 
			  (setf result-values (multiple-value-list (funcall (getf params :main-function))))
			  (general-success-exit result-values params))))
