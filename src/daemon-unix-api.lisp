(defmacro define-package-daemon-unix-api ()
  `(defpackage :daemon-unix-api 
     (:use :cl :daemon-logging :daemon-sys-linux-port)		      
     (:shadow ,@(daemon-sys-linux-port:get-unix-fn-syms))
     (:export ,@(daemon-sys-linux-port:get-unix-fn-syms)  
	      ,@(daemon-sys-linux-port:get-unix-constants))))

(define-package-daemon-unix-api)

(in-package :daemon-unix-api)

(defmacro recreate-shadow-functions (package functions-with-args)
  `(progn
     ,@(loop for sexpr in (eval functions-with-args)
	  collect (flet ((sym-in-pkg (sym &optional (package *package*) &aux (sym-name (symbol-name sym)))
			   (find-symbol sym-name package)))
		    (destructuring-bind (fn &rest args)
			sexpr
		      (let ((shadow-fn (sym-in-pkg fn))
			    (shadowed-fn (sym-in-pkg fn package)))
			`(defun-ext ,shadow-fn ,args
			   (,shadowed-fn ,@(mapcan #'(lambda (arg)
						       (cond
							 ((eq '&optional arg) nil)
							 ((consp arg) (list (first arg)))
							 ((symbolp arg) (list arg))))
						   args)))))))))
#|(macroexpand-1 '(recreate-shadow-functions
		   :daemon-sys-linux-port
		   '((open pathname flags &optional mode)
		    (exit &optional (status daemon-sys-linux-port:ex-ok))
		    (close fd))))
  |#

(recreate-shadow-functions :daemon-sys-linux-port (get-unix-functions))

#|(open
ioctl
close
grantpt
unlockpt
ptsname
dup
dup2
setsid
getpwnam

fork
sleep
exit
kill

 ;not posix				; ;
group-gid
passwd-gid
setresgid
initgroups
setresuid
prctl
cap-from-text
cap-set-proc
cap-free
enable-interrupt
)
(#:getcwd
 #:chdir
 #:umask
 #:o-rdwr #:o-rdonly #:o-creat #:o-trunc #:o-wronly 
 #:sigusr1 #:sigchld #:sigkill
 #:getpid #:getgrnam #:getppid 

		;not posix		;
 #:ex-ok #:ex-software  
 #:s-iread #:s-iroth #:s-iwrite #:+pr_set_keepcaps+ #:tiocnotty		
 #:passwd-uid
 )
		       |#