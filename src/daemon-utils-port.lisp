(defpackage :daemon-utils-port
  (:use :cl 
	:daemon-logging
	:daemon-features	
	#-linux (error "Package DAEMON-UTILS-PORT not implemented on not Linux")
	#+linux
	:daemon-utils-linux-port)  
  (:export #:restrict-rights
	   #:isolate-process
	   #:exit
	   #:get-args
	   #:getpid))	   

(in-package :daemon-utils-port)

(defun-ext change-user (name &optional group)
  #-linux (error "CHANGE-USER not implemented on not Linux")
  #+linux (linux-change-user name group))

(defun-ext preparation-before-grant ()
  #+linux 
  (progn 
    #+daemon.listen-privileged-ports
    (preparation-before-grant-listen-privileged-ports)))

(defun-ext set-grant ()
  #+linux 
  (progn 
    #+daemon.listen-privileged-ports
    (set-grant-listen-privileged-ports)))

(defun-ext restrict-rights (&key new-user new-group)  
  (preparation-before-grant)
  #+daemon.change-user
  (when new-user
    (change-user new-user new-group))
  (set-grant))


#+daemon.as-daemon
(defun-ext isolate-process ()  
    #-linux (error "ISOLATE-PROCESS not implemented on not Linux")
    #+linux
    (progn 
      (log-info "start isolate process ...")
      (detach-from-tty)
      (switch-to-slave-pseudo-terminal)
      (start-new-session)
      (log-info "... OK(isolate process).")))

