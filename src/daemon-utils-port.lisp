#-linux (error "Not implemented on not Linux")
(defpackage :daemon-utils-port
  (:use :cl :daemon-required-features :daemon-sys-port)
  (:export #:change-user
	   #:preparation-before-grant-listen-privileged-ports
	   #:preparation-before-grant
	   #:set-grant-listen-privileged-ports
	   #:set-grant
	   #:transform-into-service
	   ))

(in-package :daemon-utils-port)

(when (req-features-p :change-user)
  #+linux 
  (defun linux-change-user (name &optional group)
    (let* ((passwd (getpwnam name))
	   (gid (if group
		    (group-gid (getgrnam group))
		    (passwd-gid passwd)))
	   (uid (passwd-uid passwd)))
      (setresgid gid gid gid)
      (initgroups name gid)
      (setresuid uid uid uid))))

(when (req-features-p :change-user)
  (defun change-user (name &optional group)
    #+linux (linux-change-user name group)))

;;;;;;;;

(when (req-feature-p :listen-privileged-port)
  (defun preparation-before-grant-listen-privileged-ports ()
    #+linux 
    (prctl +PR_SET_KEEPCAPS+ 1))

  (setf (symbol-function 'preparation-before-grant-listen-privileged-ports)
	(symbol-function 'preparation-before-grant))
  
  (defun set-grant-listen-privileged-ports ()
    #+linux
    (let ((cap_p (cap-from-text "CAP_NET_BIND_SERVICE=ep")))
      (cap-set-proc cap_p)
      (cap-free cap_p)))

  (setf (symbol-function 'set-grant-listen-privileged-ports)
	(symbol-function 'set-grant)))

(when (req-features-p :as-daemon)
  #+linux
  (defmacro fork-this-process (&key 
			       child-function-before-send-success
			       child-function)
    `(progn
       (let (*status*)
	 (defun get-status () *status*)
	 (defun signal-handler (sig info context)
	   (declare (ignore info context))
	   (setf *status* sig)))
      
       (enable-interrupt sigusr1 #'signal-handler)
       (enable-interrupt sigchld #'signal-handler)

       (unless (= (fork) 0)
	 (loop
	    while (null (get-status))
	    do (sleep 0.1))
	 (quit :unix-status (if (= (get-status) sigusr1)
				0
				1)))

       (sb-sys:enable-interrupt sb-posix:sigusr1 :default)
       (sb-sys:enable-interrupt sb-posix:sigchld :default)
       ,child-function-before-send-success
       (sb-posix:kill *ppid* sb-posix:sigusr1)
       ,main-child-function))

       (defun transform-into-service (after-run-function &optional preparation-function)
	 #-linux (error "Not implemented on not Linux")
	 #+linux 
	 (fork-this-process 
	  :child-function (funcall after-run-function)
	  :child-function-before-send-success (funcall preparation-function))))
    
      