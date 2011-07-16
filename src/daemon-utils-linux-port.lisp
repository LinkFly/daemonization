(defpackage :daemon-utils-linux-port
  (:use :cl :daemon-share :daemon-unix-api-port)
  (:shadowing-import-from :daemon-unix-api-port #:open #:close)
  #+sbcl	   
  (:import-from :daemon-sbcl-sys-linux-port 
		#:enable-interrupt #:get-args
		#:stat #:stat-uid #:passwd-name #:getpwuid)
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")
  (:export #:set-current-dir #:set-umask
	   #:detach-from-tty #:switch-to-slave-pseudo-terminal #:start-new-session
	   #:preparation-before-grant-listen-privileged-ports	   
	   #:set-grant-listen-privileged-ports #:linux-change-user
	   #:fork-this-process #:create-pid-file #:read-pid-file
	   #:fork-and-parent-exit-on-child-signal
	   #:exit #:get-args #:getpid
	   #:recreate-file-allow-write-other
	   #:get-username
	   #:get-groupname
	   #:admin-user-p
	   #:equal-users
	   #:equal-groups))   

(in-package :daemon-utils-linux-port)

(defun-ext set-current-dir (path)
  (log-info "changing directory, current directory: ~A" (getcwd))
  (chdir path)
  (log-info "new current directory: ~A" (getcwd)))

(defun-ext set-umask (value)  
  (let ((old-val (umask value)))
    (log-info "setted umask: ~S, Old umask: ~S" value old-val)
    old-val))

#+daemon.as-daemon
(progn
  (defun-ext detach-from-tty ()
    (log-info "try open /dev/tty ...")
    (let ((fd (ignore-errors (open #P"/dev/tty" O-RDWR))))
      (log-info "/dev/tty file descriptor: ~S" fd)
      (when fd 
	(log-info "try dettach from tty ..." fd)
	(let ((res (ioctl fd tiocnotty)))
	  (log-info (if (>= res 0) 
			"=> success"
			"=> no success")))	
	(close fd))))

  (defun-ext switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
    (flet ((c-bit-or (&rest args)
	     (reduce #'(lambda (x y) (boole boole-ior x y))
		     args)))
	         
      (log-info "try open /dev/ptmx ...") 
      (let* ((fdm (open #P"/dev/ptmx" O-RDWR))
	     (slavename (progn 
			  (log-info "/dev/ptmx file descriptor: ~A" fdm)
			  (grantpt fdm)
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
	(close 0)
	(dup2 fds 0)
	(close 1)
	(dup2 out-fd 1)
	(close 2)
	(dup2 err-fd 2))))
	

  (defun-ext start-new-session ()
    (setsid))

  (defun-ext read-pid-file (pid-file)
    (with-open-file (s pid-file)
      (read s)))

  (defun-ext create-pid-file (pid-file)    
    (ensure-directories-exist (make-pathname :defaults pid-file
					     :name nil
					     :type nil))
    (when (probe-file pid-file)
      (call-file-exists-error pid-file))
    (with-open-file (out pid-file
			 :direction :output
			 :if-exists :error
			 :if-does-not-exist :create)
      (write (getpid) :stream out)))

  (defun-ext get-username (&aux (pid (getpid)))
    ;(getenv "USERNAME"))
    (passwd-name 
     (getpwuid 
      (stat-uid
       (stat (format nil "/proc/~A" pid))))))

  (defun-ext get-groupname ()
    (group-name (getgrgid (getgid))))

  ) ;feature :daemon.as-daemon

#+daemon.change-user
 (progn
(defun-ext admin-user-p (user)
  (string= "root" user))

(defun-ext equal-users (user1 user2)
  (string= user1 user2))

(defun-ext equal-groups (group1 group2)
  (string= group1 group2))

(defun-ext linux-change-user (&key name group 
			      &aux passwd-struct group-struct)  
  (cond
    ((and (null group) (null name))
     (return-from linux-change-user))
    (t     
     (setf passwd-struct (wrap-log (getpwnam name)))
     (unless passwd-struct (call-passwd-struct-not-found-error name))
     (when group
       (setf group-struct (wrap-log (getgrnam group)))
       (unless group-struct (call-group-struct-not-found-error group)))
     (let* ((gid (if group
		     (group-gid group-struct)
		     (passwd-gid passwd-struct)))
	    (uid (passwd-uid passwd-struct)))
       (setresgid gid gid gid)
       (initgroups name gid)
       (setresuid uid uid uid)))))	

);feature :daemon.change-user
;;;;;;;;

#+daemon.listen-privileged-ports
(progn 
  (defun-ext preparation-before-grant-listen-privileged-ports ()
    (prctl +PR_SET_KEEPCAPS+ 1))

  (defun-ext set-grant-listen-privileged-ports ()
    (let ((cap_p (cap-from-text "CAP_NET_BIND_SERVICE=ep")))      
      (cap-set-proc cap_p)
      (cap-free cap_p)))

  ) ;feature :daemon.listen-privileged-ports

#+daemon.as-daemon
(let (status exit-code)
  (defun-ext get-status () status)
  (defun-ext get-exit-code () exit-code)
  (defun-ext clear-status-and-exit-code ()
    (setf status nil exit-code nil))
  (defun-ext signal-handler (sig info context)
    (declare (ignore info context))
    (setf status sig))
  (defun-ext sigchld-handler (sig info context)
    (declare (ignore info context))
    (setf status sig)
    (setf exit-code 
	  (ldb (byte 8 8)
	       (second (multiple-value-list (wait)))))))

#+daemon.as-daemon
(defun-ext fork-and-parent-exit-on-child-signal (&optional fn-before-exit fn-exit &aux pid untime)  
  (clear-status-and-exit-code)  
  (unless (= (setf pid (fork)) 0)
    (setf *process-type* :parent)
    (setf untime (get-universal-time))
    (loop 
       while (null (get-status))
       do (sleep 0.1)
	 if (> (- (get-universal-time) untime) 
	       *timeout-daemon-response*)
	   do (call-timeout-forked-process-response-error *timeout-daemon-response*))
    (when fn-before-exit (funcall fn-before-exit pid (get-status)))
    (let ((status (if (= (get-status) sigusr1)
		       +ex-ok+
		       +ex-software+))
	  (exit-code (get-exit-code)))
      (clear-status-and-exit-code)
      (if fn-exit 
	  (funcall fn-exit status (make-extra-status :pid pid :exit-code exit-code))
	  (funcall #'exit status))))
  (setf *process-type* :child))

#+daemon.as-daemon
(defun-ext fork-this-process (&key 
			  fn-exit
			     parent-form-before-fork
			     parent-form-before-exit
			     child-form-after-fork
			     child-form-before-send-success
			     child-form-after-send-success
			     main-child-form)
  (progn
    (log-info "preparing before fork this process ...")    
    (wrap-log (enable-interrupt sigusr1 #'signal-handler)
	      (enable-interrupt sigchld #'sigchld-handler)
	      (when parent-form-before-fork (funcall parent-form-before-fork)))
    (log-info " ... OK(preparing before fork).")
    (wrap-log      
     (fork-and-parent-exit-on-child-signal parent-form-before-exit fn-exit)
     (when child-form-after-fork (funcall child-form-after-fork))
     (enable-interrupt sigusr1 :default)
     (enable-interrupt sigchld :default)
     (when child-form-before-send-success (funcall child-form-before-send-success)))

    (kill (getppid) sigusr1)

    (wrap-log (when child-form-after-send-success (funcall child-form-after-send-success))
	      (when main-child-form (funcall main-child-form)))))

(defun recreate-file-allow-write-other (file &aux fd)    
  "Function recreate file with append permissions on writing for other users.
(Cauting! This is function using for root tests and need for switch user (then writing logs with new user).
He not defining as (defun-ext ...). Cauting2!!! Function is not must using logger system (not must using
defining functions defining with defun-ext)"
  (let ((*disabled-functions-logging* '(umask open close)))
    (let ((pred-umask (umask 0)))
      (setq fd (open file (boole boole-ior O-RDWR O-CREAT) #b110110110))
      (close fd)
      (umask pred-umask))))


     