(defpackage :daemon-interfaces 
  (:use :cl :daemon-share)
  (:export #:define-provider-package
	   #:+for-daemon-unix-functions+ 
	   #:get-unix-functions #:get-unix-fn-syms #:get-unix-constants))

(in-package :daemon-interfaces)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro define-provider-package (pkg-name pkg-implementor (&key use get-fn-syms-form get-constants-form export))
    `(defpackage ,pkg-name
       (:use ,pkg-implementor ,@use)
       (:shadow ,@(eval get-fn-syms-form))
       (:export ,@(eval get-fn-syms-form)
		,@(eval get-constants-form)
		,@export)))

;;; For shadowing in daemon-unix-api 
  (define-constant +for-daemon-unix-functions+  ;(makunbound '+for-daemon-unix-functions+)
      '(
	;; IEEE Std 1003.1-2008 (POSIX.1-2008)
	o-rdwr
	o-rdonly
	o-creat 
	o-trunc 
	o-wronly 
	sigusr1
	sigchld
	sigkill
	
	(syslog priority format &rest args)
	(open pathname flags &optional mode)
	(ioctl fd cmd &optional arg)
	(close fd)
	(grantpt fd)
	(unlockpt fd)
	(ptsname fd)
	(dup oldfd)
	(dup2 oldfd newfd)
	(setsid)	

	(fork)      
	(exit &optional (status +ex-ok+))
	(kill pid signal)
	(wait &optional statusptr)

	(getcwd)
	(chdir pathname)
	(umask mode)
	(getpid)
	(getppid)

	(getpwnam login-name)
	(getgrnam login-name) 
	(getgid)
	(getgrgid gid)	

	;;not posix but getters for posix fields of group and passwd structures
	(group-gid object)
	(group-name object)	
	(passwd-gid object)	
	(passwd-uid object)
	
	;; not posix
	(initgroups user group)
	(setresgid rgid egid sgid)
	(setresuid ruid euid suid)
	s-iread
	s-iroth 
	s-iwrite
	tiocnotty		

	;; not posix - capabilities
	#+daemon.listen-privileged-ports
	(prctl option arg)
	#+daemon.listen-privileged-ports
	+pr_set_keepcaps+ 
	#+daemon.listen-privileged-ports
	(cap-from-text text)
	#+daemon.listen-privileged-ports
	(cap-set-proc cap_p)
	#+daemon.listen-privileged-ports
	(cap-free cap_p)      		
	))

  (defun get-unix-functions ()
    (remove-if #'symbolp +for-daemon-unix-functions+))
  (defun get-unix-fn-syms ()
    (mapcar #'first (get-unix-functions)))
  (defun get-unix-constants ()
    (remove-if (complement #'symbolp) +for-daemon-unix-functions+))

  ) ;eval-when
