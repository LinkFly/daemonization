(defpackage :daemon-sbcl-sys-linux-port
  (:use :cl 	
	:daemon-share
	:sb-alien
        :sb-unix)  

  (:import-from :sb-posix
		#:getpwnam #:getgrnam #:group-gid #:passwd-gid #:passwd-uid
		#:setresgid #:setresuid 					
		#:kill #:getpid #:getppid
		#:getgid #:getgrgid #:group-name
		#:chdir #:getcwd #:umask #:setsid #:dup #:dup2
		#:wait)

  (:shadowing-import-from :sb-posix
			  #:sigusr1 #:sigchld #:sigkill
			  #:O-RDWR #:O-RDONLY #:O-WRONLY #:O-CREAT #:O-TRUNC 
			  #:S-IREAD #:S-IWRITE #:S-IROTH
			  #:ioctl #:close #:syslog #:log-err #:log-info
			  #:stat #:stat-uid #:passwd-name #:getpwuid)
  ;;fork and open defined follow with using sb-posix:fork and sb-posix:open
  (:shadow #:open #:fork)

  (:import-from :sb-sys #:enable-interrupt)

  (:shadowing-import-from :sb-ext #:quit #:*posix-argv*)

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
	   #:wait #:get-args
	   #:stat #:stat-uid #:passwd-name #:getpwuid
	   #:import-sys-functions-and-constants

	   #:log-info-constant #:log-err-constant
	   
	   #:get-error-description)) 

(in-package :daemon-sbcl-sys-linux-port)

(define-symbol-macro log-info-constant #.(find-symbol (symbol-name 'log-info) *package*))
(define-symbol-macro log-err-constant #.(find-symbol (symbol-name 'log-err) *package*))

#+daemon.listen-privileged-ports 
(defconstant +PR_SET_KEEPCAPS+ 8)

#+daemon.as-daemon
(let* ((pkg :sb-unix)
       (sym (find-symbol "TIOCNOTTY" pkg)))
  (defconstant tiocnotty
    (if (and sym (boundp sym))
	(symbol-value sym)
	21538)))
  
(defun get-args () *posix-argv*)

;;;;;;;;;;;;;;;;;;;;;; Logging ;;;;;;;;;;;;;;;;;;;;;;
(defmacro log-info-load (log-str &rest args) 
  `(let ((*fn-log-info* *fn-log-info-load*))
     (daemon-share:log-info ,log-str ,@args)
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Correct open (for handling mode-t param equal nil)
(defun open (pathname flags &optional mode)
  (apply #'sb-posix:open 
	 pathname
	 flags 
	 (if mode (list mode) nil)))

(defun fork ()
  (let ((fork-res (sb-posix:fork)))
    (setf *process-type*
	  (if (= fork-res 0) :child :parent))
    (daemon-share:log-info "-- here was fork --")
    fork-res))

;;;;;;;;;;;;;;;;;; Compilation stage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load library "libcap" for definition cap-from-text, cap-set-proc and cap-free
#+daemon.listen-privileged-ports
(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *libcap-probable-files* '("/lib/libcap.so.2" "/lib/libcap.so"))
  (load-shared-object (find-if #'probe-file *libcap-probable-files*)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
(defun exit (&optional (status +ex-ok+))
  (quit :unix-status status))

(defmacro def-alien-call (name &rest args &aux fn-str-name)
  (setq fn-str-name (string-upcase 
		     (substitute #\- #\_ name)))
  `(progn 
     (log-info-load "try defining ~A ..." ,name)       
     (sb-posix::define-call ,name ,@args)
     (let ((fn-sym (find-symbol (string-upcase ,fn-str-name) :sb-posix))
	   (fn-using-sym (read-from-string ,fn-str-name)))
       (setf (symbol-function fn-using-sym) (symbol-function fn-sym))
       (log-info-load " ... OK. (symbol-function '~S) => ~S"
	       fn-using-sym (symbol-function fn-using-sym)))))

(defun import-sys-functions-and-constants ()
  (let ((*package* (find-package :daemon-sbcl-sys-linux-port)))

;; Define initgroups
#+daemon.change-user
(def-alien-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))
     
;; Define constant +PR_SET_KEEPCAPS+ (already defined in begin), functions prctl,
;;  functions for grant capabilities: cap-from-text, cap-set-proc, cap-free
#+daemon.listen-privileged-ports 
(progn 
  (def-alien-call "prctl" int minusp (option int) (arg int))
  ;; For compilation following functions, "libcap.so" library must be loaded into 
  ;; compiling system (look at the begining)
  (def-alien-call "cap_from_text" (* char) null-alien (text c-string))
  (def-alien-call "cap_set_proc" int minusp (cap_p (* char)))
  (def-alien-call "cap_free" int minusp (cap_p (* char)))
  ) ;progn for :daemon.listen-privileged-ports feature

;; Define functions: "grantpt", "unlockpt", and "ptsname". 
;; Also "tiocnotty" constant (already defined in begin).
#+daemon.as-daemon
(progn 
  (def-alien-call "grantpt" int minusp (fd sb-posix::file-descriptor))
  (def-alien-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
  (def-alien-call "ptsname" c-string null (fd sb-posix::file-descriptor))      	 
  ) ;progn for :daemon.as-daemon feature
)) ;let, defun import-sys-functions-and-constants


;;;;;;;;;;;;;;;;;;;;;;; For debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *frames-before-error-call* 5)

(defmacro get-function-source (backtrace)
			      `(sb-introspect:find-definition-source
				(symbol-function
				 (first (first (nthcdr *frames-before-error-call*
						       ,backtrace))))))

(defmacro get-error-description (err)
  `(let ((backtrace-list (sb-debug:backtrace-as-list *backtrace-count*))
	 (error-description (make-error-description)))
     (with-accessors ((condition error-description-condition)
		      (backtrace error-description-backtrace)
		      (source error-description-source))
	 error-description
       (setf condition ,err
	     backtrace backtrace-list
	     source (get-function-source backtrace)))
     error-description))


    

