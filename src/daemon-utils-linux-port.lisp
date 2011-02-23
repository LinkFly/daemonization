(defpackage :daemon-utils-linux-port
  (:use :cl :daemon-features :daemon-sys-linux-port)
  (:shadowing-import-from :daemon-sys-linux-port #:open #:close)
  (:export #:set-current-dir #:set-umask
	   #:detach-from-tty #:switch-to-slave-pseudo-terminal #:start-new-session
	   #:preparation-before-grant-listen-privileged-ports	   
	   #:set-grant-listen-privileged-ports #:linux-change-user
	   #:exit #:fork-this-process #:create-pid-file #:read-pid-file
	   #:syslog-error #:syslog-info
	   ))


(in-package :daemon-utils-linux-port)
  
(defun set-current-dir (path)
  (chdir path))

(defun set-umask (value)
  (umask value))

#+daemon.as-daemon
(progn
  (defun detach-from-tty ()
    (let ((fd (ignore-errors (open #P"/dev/tty" O-RDWR))))
      (when fd 
	(ioctl fd tiocnotty)
	(close fd))))

  (defun switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
    (flet ((c-bit-or (&rest args)
	     (reduce #'(lambda (x y) (boole boole-ior x y))
		     args)))
      (let* ((fdm (open #P"/dev/ptmx" O-RDWR))
	     (slavename (progn (grantpt fdm)
			       (unlockpt fdm)
			       (ptsname fdm)))
	     (fds (open slavename O-RDONLY))
	     (out-fd (open out 
			   (c-bit-or O-WRONLY O-CREAT O-TRUNC)
			   (c-bit-or S-IREAD S-IWRITE S-IROTH)))
	     (err-fd (if (not (equal err out))
			 (open err 
			       (c-bit-or O-WRONLY O-CREAT O-TRUNC)
			       (c-bit-or S-IREAD S-IWRITE S-IROTH))
			 (if out (dup out-fd)))))
	(dup2 fds 0)
	(dup2 out-fd 1)
	(dup2 err-fd 2))))

  (defun start-new-session ()
    (setsid))

  (defun read-pid-file (pid-file)
    (with-open-file (s pid-file)
      (read s)))

  (defun create-pid-file (pid-file)
    (with-open-file (out pid-file
			 :direction :output
			 :if-exists :error
			 :if-does-not-exist :create)
      (write (getpid) :stream out)))
  ) ;feature :daemon.as-daemon

#+daemon.change-user
(defun linux-change-user (name &optional group)
  (let* ((passwd (getpwnam name))
	 (gid (if group
		  (group-gid (getgrnam group))
		  (passwd-gid passwd)))
	 (uid (passwd-uid passwd)))
    (setresgid gid gid gid)
    (initgroups name gid)
    (setresuid uid uid uid)))		;feature :daemon.change-user
;;;;;;;;

#+daemon.listen-privileged-ports
(progn 
  (defun preparation-before-grant-listen-privileged-ports ()
    #+linux 
    (prctl +PR_SET_KEEPCAPS+ 1)) 

  (defun set-grant-listen-privileged-ports ()
    #+linux
    (let ((cap_p (cap-from-text "CAP_NET_BIND_SERVICE=ep")))
      (cap-set-proc cap_p)
      (cap-free cap_p)))

  ) ;feature :daemon.listen-privileged-ports

#+daemon.as-daemon
(defmacro fork-this-process (&key 
			     parent-form-before-fork
			     child-form-after-fork
			     child-form-before-send-success
			     main-child-form)
  `(progn
     (let (status)
       (defun get-status () status)
       (defun signal-handler (sig info context)
	 (declare (ignore info context))
	 (setf status sig)))
      
     (enable-interrupt sigusr1 #'signal-handler)
     (enable-interrupt sigchld #'signal-handler)
     
     ,parent-form-before-fork

     (unless (= (fork) 0)
       (loop
	  while (null (get-status))
	  do (sleep 0.1))
       (exit (if (= (get-status) sigusr1)
		 ex-ok
		 ex-software)))

     ,child-form-after-fork
     (enable-interrupt sigusr1 :default)
     (enable-interrupt sigchld :default)
     ,child-form-before-send-success
     (kill (getppid) sigusr1)
     ,main-child-form))

;;; Logging
(defun syslog (log-type format-str &rest args)
  (syslog log-type (apply #'format nil format-str args)))

(defun syslog-error (format-str &rest args)
  (apply #'syslog log-err format-str args))

(defun syslog-info (format-str &rest args)
  (apply #'syslog log-info format-str args))