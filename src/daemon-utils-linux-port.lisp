(defpackage :daemon-utils-linux-port
  (:use :cl :daemon-logging :daemon-sys-linux-port)
  ;(:shadow #:log-info #:log-err)
  (:shadowing-import-from :daemon-sys-linux-port #:open #:close)
  (:export #:set-current-dir #:set-umask
	   #:detach-from-tty #:switch-to-slave-pseudo-terminal #:start-new-session
	   #:preparation-before-grant-listen-privileged-ports	   
	   #:set-grant-listen-privileged-ports #:linux-change-user
	   #:exit #:fork-this-process #:create-pid-file #:read-pid-file
	   #:*fn-log-info* #:*fn-log-err*
	   ))

(in-package :daemon-utils-linux-port)
;(unintern (find-symbol "LOG-INFO"))
;(unintern (find-symbol "LOG-ERR"))
;;; Correct log-info and log-err (function from :daemon-logging, value from :daemon-sys-linux-port)
;(setf (symbol-function 'log-info) (symbol-function 'daemon-logging:log-info))
;(setf log-info daemon-sys-linux-port:log-info)
;(setf (symbol-function 'log-err) (symbol-function 'daemon-logging:log-err))
;(setf log-err daemon-sys-linux-port:log-err)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging
;(defconstant +log-layer+ :os-linux-layer)
;(defparameter *fn-log-info* #'(lambda (fmt-str &rest args)
;				(syslog log-info (princ (apply #'format nil fmt-str args)))))
;(defparameter *fn-log-err* #'(lambda (fmt-str &rest args)
;				(syslog log-err (princ (concatenate 'string "ERR: " (apply #'format nil fmt-str args))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging checking
;(log-info "test")
;(defun-ext f (x y) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4)
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun-ext set-current-dir (path)
  (log-info "changing directory, current directory: ~S ..." path (getcwd))
  (chdir path)
  (log-info "new current directory: ~S." (getcwd)))

(defun-ext set-umask (value)
  (let ((old-val (umask value)))
    (log-info "setted umask: ~S, Old umask: ~S." value old-val)
    old-val))

#+daemon.as-daemon
(progn
  (defun-ext detach-from-tty ()
    (log-info "try open /dev/tty ...")
    (let ((fd (ignore-errors (wrap-log (open #P"/dev/tty" O-RDWR)))))
      (log-info "/dev/tty file descriptor: ~S" fd)
      (when fd 
	(log-info "try dettach from tty ..." fd)
	(let ((res (wrap-log (ioctl fd tiocnotty))))
	  (log-info (if (> res 0) 
			"=> success."
			"=> no success.")))
	(wrap-log (close fd)))))

  (defun-ext switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
    (flet ((c-bit-or (&rest args)
	     (reduce #'(lambda (x y) (boole boole-ior x y))
		     args)))      
      (log-info "try open /dev/ptmx ...") 
      (let* ((fdm (wrap-log (open #P"/dev/ptmx" O-RDWR))) 
	     (slavename (progn 
			  (log-info "~S: /dev/ptmx file descriptor" fdm)
			  (wrap-log 
			   (grantpt fdm)
			   (unlockpt fdm)
			   (ptsname fdm))))
	     (fds (wrap-log (open slavename O-RDONLY)))
	     (out-fd (wrap-log (open out 
				     (c-bit-or O-WRONLY O-CREAT O-TRUNC)
				     (c-bit-or S-IREAD S-IWRITE S-IROTH))))
	     (err-fd (if (not (equal err out))
			 (wrap-log (open err 
					 (c-bit-or O-WRONLY O-CREAT O-TRUNC)
					 (c-bit-or S-IREAD S-IWRITE S-IROTH)))
			 (if out (wrap-log (dup out-fd))))))
	(wrap-log (dup2 fds 0))
	(wrap-log (dup2 out-fd 1))
	(wrap-log (dup2 err-fd 2)))))

  (defun-ext start-new-session ()
    (wrap-log (setsid)))

  (defun-ext read-pid-file (pid-file)
    (with-open-file (s pid-file)
      (read s)))

  (defun-ext create-pid-file (pid-file)
    (with-open-file (out pid-file
			 :direction :output
			 :if-exists :error
			 :if-does-not-exist :create)
      (write (getpid) :stream out)))
  ) ;feature :daemon.as-daemon

#+daemon.change-user
(defun-ext linux-change-user (name &optional group)
  (let* ((passwd (wrap-log (getpwnam name)))
	 (gid (if group
		  (wrap-log (group-gid (wrap-log (getgrnam group))))
		  (wrap-log (passwd-gid passwd))))
	 (uid (wrap-log (passwd-uid passwd))))
    (wrap-log (setresgid gid gid gid))
    (wrap-log (initgroups name gid))
    (wrap-log (setresuid uid uid uid))))	;feature :daemon.change-user
;;;;;;;;

#+daemon.listen-privileged-ports
(progn 
  (defun-ext preparation-before-grant-listen-privileged-ports ()
    #+linux 
    (wrap-log (prctl +PR_SET_KEEPCAPS+ 1)))

  (defun-ext set-grant-listen-privileged-ports ()
    #+linux
    (let ((cap_p (wrap-log (cap-from-text "CAP_NET_BIND_SERVICE=ep"))))
      (wrap-log (cap-set-proc cap_p))
      (wrap-log (cap-free cap_p))))

  ) ;feature :daemon.listen-privileged-ports

#+daemon.as-daemon
(defmacro fork-this-process (&key 
			     parent-form-before-fork
			     child-form-after-fork
			     child-form-before-send-success
			     main-child-form)
  `(progn
     (let (status)
       (defun-ext get-status () status)
       (defun-ext signal-handler (sig info context)
	 (declare (ignore info context))
	 (setf status sig)))
      
     (wrap-log (enable-interrupt sigusr1 #'signal-handler)
	       (enable-interrupt sigchld #'signal-handler)
	       ,parent-form-before-fork)

     (log-info "fork proceed ...")
     (unless (= (wrap-log (fork)) 0)
       (loop
	  while (null (get-status))
	  do (wrap-log (sleep 0.1)))
       (wrap-log (exit (if (= (get-status) sigusr1)
			   ex-ok
			   ex-software))))

     (wrap-log ,child-form-after-fork
	       (enable-interrupt sigusr1 :default)
	       (enable-interrupt sigchld :default)
	       ,child-form-before-send-success
	       (kill (wrap-log (getppid)) sigusr1)
	       ,main-child-form)))