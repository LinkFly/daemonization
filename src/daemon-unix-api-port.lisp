#|(defmacro define-package-daemon-unix-api ()
  `(defpackage :daemon-unix-api 
     (:use :cl :daemon-logging :daemon-interfaces
	   #+sbcl	   
	   :daemon-sbcl-sys-linux-port
	   #-sbcl
	   (error "Not implemented for non sbcl lisp systems"))
     (:shadow ,@(daemon-sys-linux-port:get-unix-fn-syms))
     (:export ,@(daemon-sys-linux-port:get-unix-fn-syms)  
	      ,@(daemon-sys-linux-port:get-unix-constants))))
|#
#|(macroexpand-1 '(daemon-interfaces:define-provider-package 
    :daemon-unix-api
  #+sbcl
  :daemon-sbcl-sys-linux-port
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")
  (:use (:cl :daemon-logging :daemon-interfaces)
	:get-fn-syms-form (daemon-interfaces:get-unix-fn-syms)
	:get-constants-form (daemon-interfaces:get-unix-constants))))
|#

(daemon-interfaces:define-provider-package 
    :daemon-unix-api-port
  #+sbcl
  :daemon-sbcl-sys-linux-port
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")
  (:use (:cl :daemon-logging :daemon-interfaces)
   :get-fn-syms-form (daemon-interfaces:get-unix-fn-syms)
   :get-constants-form (daemon-interfaces:get-unix-constants)))

(in-package :daemon-unix-api-port)

(defmacro recreate-shadow-functions (package functions-with-args)
  `(progn
     ,@(loop for sexpr in (eval functions-with-args)
	  collect (flet ((sym-in-pkg (sym &optional (package *package*) &aux (sym-name (symbol-name sym)))
			   (find-symbol sym-name package)))
		    (destructuring-bind (fn &rest args)
			sexpr
		      (let ((shadow-fn (sym-in-pkg fn))
			    (shadowed-fn (sym-in-pkg fn package)))
			`(defun-ext ,shadow-fn ,(mapcar #'(lambda (arg)
							    (cond
							      ((eq '&optional arg) '&optional)
							      ((consp arg) (list (progn (intern (symbol-name (first arg)) *package*)
											(sym-in-pkg (first arg)))
										 (second arg)))
							      ((symbolp arg) 
							       (intern (symbol-name arg) *package*)
							       (sym-in-pkg arg))))
							args)
			   (,shadowed-fn ,@(mapcan #'(lambda (arg)
						       (cond
							 ((eq '&optional arg) nil)
							 ((consp arg) (list (sym-in-pkg (first arg))))
							 ((symbolp arg) (list (sym-in-pkg arg)))))
						   args)))))))))

;(macroexpand-1 '(recreate-shadow-functions :daemon-sbcl-sys-linux-port (daemon-interfaces:get-unix-functions)))
(recreate-shadow-functions 
   #+sbcl
  :daemon-sbcl-sys-linux-port
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")
  (daemon-interfaces:get-unix-functions)) 


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