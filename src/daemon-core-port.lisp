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
	   #:define-unix-functions))

(in-package :daemon-core-port)

(defun logging-init ()
  (defparameter *fn-log-info* #'(lambda (fmt-str &rest args)
				  (add-daemon-log (apply #'format nil fmt-str args))
				  (apply 'syslog-info fmt-str args)))
  (defparameter *fn-log-info-load* nil)
  (defparameter *fn-log-err* #'(lambda (fmt-str &rest args)
				 (add-daemon-log (concatenate 'string "ERROR: " (apply #'format nil fmt-str args)))
				 (apply 'syslog-err (concatenate 'string "ERROR: " fmt-str) args)))
  (defparameter *fn-log-trace* #'(lambda (fmt-str)
				   (syslog-info "~A" (add-daemon-log fmt-str))))
  (defparameter *fn-log-pid* #'(lambda () (let ((*print-call* nil)) (getpid)))))

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
			      (restrict-rights :new-user (getf params :user) 
					       :new-group (getf params :group)))
     :preparation-fn #'isolate-process
     :before-init-fn (getf params :before-init-fn)
     :main-fn (getf params :main-function)
     :os-params (getf params :os-params)))
  
  ) ;feature :as-daemon					

(defun-ext simple-start (params)
  (restrict-rights :new-user (getf params :user) 
		   :new-group (getf params :group))
  (start-as-no-daemon (getf params :main-function)))
