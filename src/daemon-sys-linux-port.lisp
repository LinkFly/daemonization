(defpackage :daemon-sys-linux-port
  (:use :cl 
	:daemon-features
	#+sbcl :sb-alien
        #+sbcl :sb-unix)
  #+sbcl
  (:import-from :sb-posix
		#:getpwnam #:getgrnam #:group-gid #:passwd-gid #:passwd-uid
		#:setresgid #:setresuid #:fork #:kill #:getpid #:getppid
		#:chdir #:umask #:setsid #:dup #:dup2)
  #+sbcl
  (:shadowing-import-from :sb-posix
			  #:sigusr1 #:sigchld #:sigkill #:open 
			  #:O-RDWR #:O-RDONLY #:O-WRONLY #:O-CREAT #:O-TRUNC 
			  #:S-IREAD #:S-IWRITE #:S-IROTH
			  #:ioctl #:close #:syslog #:log-err #:log-info)
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
	   #:chdir #:umask #:setsid #:ioctl #:close
	   #:dup #:dup2))

(in-package :daemon-sys-linux-port)

(defconstant ex-ok 0)
(defconstant ex-software 70)
#+(and sbcl daemon.listen-privileged-ports)
(defparameter *libcap-probable-files* '("/lib/libcap.so.2" "/lib/libcap.so"))

(defun exit (&optional (status ex-ok))
  #-sbcl (error "Not implemented on not sbcl lisps")
  #+sbcl (quit status))

#-sbcl (error "DEF-ALIEN-CALL not implemented for not SBCL lisps")
#+sbcl
(defmacro def-alien-call (name &rest args &aux fn-str-name)

  (setq fn-str-name (string-upcase 
		     (substitute #\- #\_ name)))
  `(progn (sb-posix::define-call ,name ,@args)
	  (setf (symbol-function (read-from-string ,fn-str-name))
		(symbol-function 
		 (find-symbol ,fn-str-name :sb-posix)))))
			      
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
    (load-shared-object (find-if #'probe-file *libcap-probable-files*))
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
    
  (unless (boundp 'tiocnotty)
    (defconstant tiocnotty 21538))
  ) ;progn for :daemon.as-daemon feature


