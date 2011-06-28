(defpackage :daemon-interfaces 
  (:use :cl :daemon-share)
  (:export #:define-provider-package
	   #:+for-daemon-unix-functions+ 
	   #:get-unix-functions #:get-unix-fn-syms #:get-unix-constants))

(in-package :daemon-interfaces)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-provider-package (pkg-name pkg-implementor (&key use get-fn-syms-form get-constants-form))
    `(defpackage ,pkg-name
       (:use ,pkg-implementor ,@use)
       (:shadow ,@(eval get-fn-syms-form))
       (:export ,@(eval get-fn-syms-form)
		,@(eval get-constants-form))))

;;; For shadowing in daemon-unix-api 
  (define-constant +for-daemon-unix-functions+
      '((open pathname flags &optional mode)
	(ioctl fd cmd &optional arg)
	(close fd)
	(grantpt fd)
	(unlockpt fd)
	(ptsname fd)
	(dup oldfd)
	(dup2 oldfd newfd)
	(setsid)
	(getpwnam login-name)

	(fork)      
	(exit &optional (status ex-ok))
	(kill pid signal)

	(getcwd)
	(chdir pathname)
	(umask mode)
	(getpid)
	(getgrnam login-name) 
	(getppid)

	(wait &optional statusptr)

	o-rdwr
	o-rdonly
	o-creat 
	o-trunc 
	o-wronly 
	sigusr1
	sigchld
	sigkill
 
					;not posix
	(group-gid object)
	(passwd-gid object)
	(setresgid rgid egid sgid)
	(initgroups user group)
	(setresuid ruid euid suid)
	
	#+daemon.listen-privileged-ports
	(prctl option arg)
	#+daemon.listen-privileged-ports
	(cap-from-text text)
	#+daemon.listen-privileged-ports
	(cap-set-proc cap_p)
	#+daemon.listen-privileged-ports
	(cap-free cap_p)      
      
	(passwd-uid object)

	s-iread
	s-iroth 
	s-iwrite
	+pr_set_keepcaps+ 
	tiocnotty		
	))

  (defun get-unix-functions ()
    (remove-if #'symbolp +for-daemon-unix-functions+))
  (defun get-unix-fn-syms ()
    (mapcar #'first (get-unix-functions)))
  (defun get-unix-constants ()
    (remove-if (complement #'symbolp) +for-daemon-unix-functions+))

  ) ;eval-when
