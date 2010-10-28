#-(and sbcl linux) (error "Not implemented for not SBCL lisps and not Linux"))
(defpackage :daemon-sys-port
  (:use :cl 
	:daemon-required-features
	#+(and sbcl linux) :sb-alien)
  #+(and sbcl linux)
  (:import-from :sb-posix
		#:getpwnam 
		#:getgrnam
		#:group-gid
		#:passwd-gid
		#:passwd-uid
		#:setresgid
		#:setresuid

		#:fork
		#:kill
		#:sigusr1
		#:sigchld)

  #+(and sbcl linux)
  (:import-from :sb-sys #:enable-interrupt)
  (:export #:getpwnam 
	   #:getgrnam
	   #:group-gid
	   #:passwd-gid
	   #:passwd-uid
	   #:setresgid
	   #:setresuid

	   #:fork
	   #:kill
	   #:sigusr1
	   #:sigchld
	   #:enable-interrupt

	   #:initgroups
	   
	   #:+PR_SET_KEEPCAPS+
	   #:prctl

	   #:cap_from_text
	   #:cap_set_proc
	   #:cap_free

	   #:grantpt
	   #:unlockpt
	   #:ptsname))
	   

(in-package :daemon-sys-port)

#+(and sbcl linux)
(progn
  (defmacro def-alien-call (name &rest args)
    `(progn (sb-posix::define-call ,name ,@args)
	    (setf (symbol-function (read-from-string ,name))
		  (symbol-function 
		   (find-symbol (string-upcase 
				 (substitute #\- #\_ ,name))
				:sb-posix)))))
      

  (when (req-feature-p :change-user) 
    (def-alien-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t)))

  (when (req-feature-p :listen-privileged-port)
    (defconstant +PR_SET_KEEPCAPS+ 8)
    (def-alien-call "prctl" int minusp (option int) (arg int))

    (load-shared-object (find-if #'probe-file
				 '("/lib/libcap.so.2" "/lib/libcap.so")))
    (def-alien-call "cap_from_text" (* char) null-alien (text c-string))
    (def-alien-call "cap_set_proc" int minusp (cap_p (* char)))
    (def-alien-call "cap_free" int minusp (cap_p (* char))))

  (when (req-feature-p :switch-to-pts)
    (def-alien-call "grantpt" int minusp (fd sb-posix::file-descriptor))
    (def-alien-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
    (def-alien-call "ptsname" c-string null (fd sb-posix::file-descriptor))))






