(defpackage :daemon-sys-linux-port
  (:use :cl 	
	:daemon-logging
	#+sbcl :sb-alien
        #+sbcl :sb-unix)  
  #+sbcl
  (:import-from :sb-posix
		#:getpwnam #:getgrnam #:group-gid #:passwd-gid #:passwd-uid
		#:setresgid #:setresuid 					
		#:kill #:getpid #:getppid
		#:chdir #:getcwd #:umask #:setsid #:dup #:dup2
		#:wait)
  #+sbcl
  (:shadowing-import-from :sb-posix
			  #:sigusr1 #:sigchld #:sigkill
			  #:O-RDWR #:O-RDONLY #:O-WRONLY #:O-CREAT #:O-TRUNC 
			  #:S-IREAD #:S-IWRITE #:S-IROTH
			  #:ioctl #:close #:syslog #:log-err #:log-info)
  ;;fork and open defined follow with using sb-posix:fork and sb-posix:open
  (:shadow #:open #:fork)
  #+sbcl
  (:import-from :sb-sys #:enable-interrupt)

  #+sbcl
  (:shadowing-import-from :sb-ext #:quit)

  (:export #:getpwnam #:getgrnam #:group-gid #:passwd-gid #:passwd-uid #:setresgid #:setresuid
	   #:fork #:kill #:sigusr1 #:sigchld #:sigkill #:enable-interrupt :initgroups
	   #:+PR_SET_KEEPCAPS+ #:prctl #:cap-from-text #:cap-set-proc #:cap-free
	   #:grantpt #:unlockpt #:ptsname #:exit #:open
	   #:O-RDWR #:O-RDONLY #:O-WRONLY #:O-CREAT
	   #:O-TRUNC #:S-IREAD #:S-IWRITE #:S-IROTH
	   #:getpid #:getppid #:ex-ok #:ex-software 
	   #:chdir #:getcwd #:umask #:setsid #:ioctl #:close
	   #:dup #:dup2
	   #:tiocnotty #:syslog
	   #:get-unix-functions #:get-unix-fn-syms #:get-unix-constants
	   #:wait
	   ))

(in-package :daemon-sys-linux-port)

;;;;;;;;;;;;;;;;;;;;;; Logging ;;;;;;;;;;;;;;;;;;;;;;

;;; Correct log-info and log-err (function from :daemon-logging)
(eval-when (:compile-toplevel)
  (setf (macro-function 'log-info) (macro-function 'daemon-logging:log-info))
  (setf (macro-function 'log-err) (macro-function 'daemon-logging:log-err)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter daemon-logging:*fn-log-info* #'(lambda (fmt-str &rest args)
				(syslog log-info (add-daemon-log (apply #'format nil fmt-str args)))))
(defparameter daemon-logging:*fn-log-err* #'(lambda (fmt-str &rest args)
				(syslog log-err (add-daemon-log (concatenate 'string "ERROR: " (apply #'format nil fmt-str args))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Correct open (for handling mode-t param equal nil
(defun open (pathname flags &optional mode)
  (apply #'sb-posix:open 
	 pathname
	 flags 
	 (if mode (list mode) nil)))

(defun fork ()
  (let ((fork-res (funcall #'sb-posix:fork)))
    (log-info "-- here was fork --")
    (setf *log-prefix* 
	  (if (= fork-res 0) (list :child-proc (getpid)) (list :parent-proc (getpid))))
    fork-res))

;;;;;;;;;;;;;;;;;; Compilation stage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For shadowing in daemon-unix-api 
(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

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
      (prctl option arg)
      (cap-from-text text)
      (cap-set-proc cap_p)
      (cap-free cap_p)      
      
      (passwd-uid object)
      ex-ok
      ex-software  
      s-iread
      s-iroth 
      s-iwrite
      +pr_set_keepcaps+ 
      tiocnotty		
      ))

(eval-when (:compile-toplevel :load-toplevel)
  (defun get-unix-functions ()
    (remove-if #'symbolp +for-daemon-unix-functions+))
  (defun get-unix-fn-syms ()
    (mapcar #'first (get-unix-functions)))
  (defun get-unix-constants ()
    (remove-if (complement #'symbolp) +for-daemon-unix-functions+))

  #|(defun get-unix-symbols () 
    (mapcar #'(lambda (form)
		(typecase form
		  (cons (first form))
		  (symbol form)))
	    +for-daemon-unix-functions+))|#
  )

;; For definition cap-from-text, cap-set-proc and cap-free
#+(and sbcl daemon.listen-privileged-ports)
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *libcap-probable-files* '("/lib/libcap.so.2" "/lib/libcap.so"))
  (load-shared-object (find-if #'probe-file *libcap-probable-files*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant ex-ok 0)
(defconstant ex-software 70)

;;;;;;;;;;;
(defun exit (&optional (status ex-ok))
  #-sbcl (error "Not implemented on not sbcl lisps")
  #+sbcl (quit :unix-status status))

#-sbcl (error "DEF-ALIEN-CALL not implemented for not SBCL lisps")
#+sbcl
(defmacro def-alien-call (name &rest args &aux fn-str-name)
  (setq fn-str-name (string-upcase 
		     (substitute #\- #\_ name)))
  `(progn 
     (log-info "try defining ~A ..." ,name)       
     (sb-posix::define-call ,name ,@args)
     (let ((fn-sym (find-symbol (string-upcase ,fn-str-name) :sb-posix))
	   (fn-using-sym (read-from-string ,fn-str-name)))
       (setf (symbol-function fn-using-sym) (symbol-function fn-sym))
       (log-info " ... OK. (symbol-function '~S) => ~S"
	       fn-using-sym (symbol-function fn-using-sym)))))

;; Define initgroups
#+daemon.change-user
(progn 
  #-sbcl (error "Not implemented for not SBCL lisps")
  #+sbcl (def-alien-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))
) ;progn for :daemon.listen-privileged-ports feature
     
;; Define constant +PR_SET_KEEPCAPS+, functions prctl, load library "libcap", and
;;  functions for grant capabilities: cap-from-text, cap-set-proc, cap-free
#+daemon.listen-privileged-ports 
(progn 
  (defconstant +PR_SET_KEEPCAPS+ 8)
  (def-alien-call "prctl" int minusp (option int) (arg int))
  #-sbcl
  (error "Not implemented load libcap library (with cap_xx functions) for not sbcl lisps")
  #+sbcl 
  (progn     
    ;; For compilation following functions, "libcap.so" library must be loaded into 
    ;; compiling system (look at the begining)
    (def-alien-call "cap_from_text" (* char) null-alien (text c-string))
    (def-alien-call "cap_set_proc" int minusp (cap_p (* char)))
    (def-alien-call "cap_free" int minusp (cap_p (* char))))

  ) ;progn for :daemon.listen-privileged-ports feature

;; Define functions: "grantpt", "unlockpt", and "ptsname". Also "tiocnotty" constant.
#+daemon.as-daemon
(progn 
  #-sbcl (error "Not implemented functions grantpt, unlockpt, and ptsname for not SBCL lisps")
  #+sbcl
  (progn 
    (def-alien-call "grantpt" int minusp (fd sb-posix::file-descriptor))
    (def-alien-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
    (def-alien-call "ptsname" c-string null (fd sb-posix::file-descriptor)))
    
  (let* ((pkg 
	  #+sbcl :sb-unix 
	  #-sbcl (error "Not implemented search TIOCNOTTY constant for not SBCL lisps")
	  )
	 (sym (find-symbol "TIOCNOTTY" pkg)))
    (defconstant tiocnotty
      (if (and sym (boundp sym))
	  (symbol-value sym)
	  21538)))
    
  ) ;progn for :daemon.as-daemon feature


