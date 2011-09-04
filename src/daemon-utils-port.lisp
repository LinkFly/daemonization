(defpackage :daemon-utils-port
  (:use :cl 
	:daemon-share
	#-linux (error "Package DAEMON-UTILS-PORT not implemented on not Linux")
	#+linux
	:daemon-utils-linux-port)  
  (:export #:restrict-rights
	   #:isolate-process
	   #:exit
	   #:get-args
	   #:getpid
	   #:recreate-file-allow-write-other
	   #:admin-current-user-p
	   #:safe-write))	   

(in-package :daemon-utils-port)

(defun-ext change-user-and-group (&key user group)
  #-linux (error "CHANGE-USER-AND-GROUP not implemented on not Linux")
  #+linux (linux-change-user-and-group :name user :group group))

(defun-ext preparation-before-grant ()
  #+linux 
  (progn 
    #+daemon.listen-privileged-ports
    (when *listen-privileged-ports*
      (preparation-before-grant-listen-privileged-ports))))

(defun-ext set-grant ()
  #+linux 
  (progn 
    #+daemon.listen-privileged-ports
    (when *listen-privileged-ports*
      (set-grant-listen-privileged-ports))))

(defun-ext admin-current-user-p ()
  (admin-user-p (get-username)))

(defun-ext is-admin-user-and-change-user-and-group-p (new-user new-group)
  (when (and (null new-user) (null new-group))
    (return-from is-admin-user-and-change-user-and-group-p))
  (let* ((cur-user (get-username))
	 (cur-group (get-groupname))
	 (is-new-user-current (equal-users cur-user new-user)))
    (when (and (or is-new-user-current (null new-user))
	       (and new-group (not (equal-groups cur-group new-group))))
      (call-group-change-but-user-not-change-error new-group))
    (unless (or (admin-user-p cur-user)
	        is-new-user-current)
      (call-denied-change-user-error cur-user new-user))
    (and (admin-user-p cur-user)
	 (not is-new-user-current)))) 
     
(defun-ext restrict-rights (&key new-user new-group)      
  #+daemon.change-user-and-group
  (when (is-admin-user-and-change-user-and-group-p new-user new-group)  
    (preparation-before-grant)        
    (change-user-and-group :user new-user :group new-group)
    (set-grant)))

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

