;;;; daemonization.asd
;;;;
;;;; This file is part of the common lisp library - daemonization, released under Lisp-LGPL.
;;;; Based on restas-daemon.lisp (from contrib RESTAS system).
;;;; See file COPYING for details.
;;;;
;;;; Author: Katrevich Sergey <linkfly1@newmail.ru>

(defsystem :daemonization
  :version "0.0.1"
  :depends-on (#+sbcl :sb-posix
	       #-sbcl #.(error "Not implemented for non sbcl lisp systems"))
  :components ((:module "src"
			:components ((:file "daemon-features")
				     (:file "daemon-logging")				     
				     (:file "daemon-share" :depends-on ("daemon-features" "daemon-logging"))
				     (:file "daemon-interfaces" :depends-on ("daemon-share"))
				     #+(and linux sbcl) 
				     (:file "daemon-sbcl-sys-linux-port" :depends-on ("daemon-share"))
				     #+linux 
				     (:file "daemon-unix-api-port" :depends-on ("daemon-share" "daemon-interfaces" 
											    #+sbcl
											    "daemon-sbcl-sys-linux-port"
											    #-sbcl 
											    #.(error "Not implemented for non sbcl lisp systems")))
				     #+linux 
				     (:file "daemon-utils-linux-port" :depends-on ("daemon-share" 
										   #+sbcl
										   "daemon-sbcl-sys-linux-port"
										   #-sbcl 
										   #.(error "Not implemented for non sbcl lisp systems")
										   "daemon-unix-api-port"))
				     #+linux 
				     (:file "daemon-core-linux-port" :depends-on ("daemon-share"
										  #+sbcl
										   "daemon-sbcl-sys-linux-port"
										   #-sbcl 
										   #.(error "Not implemented for non sbcl lisp systems")
										  "daemon-unix-api-port"
										  "daemon-utils-linux-port"))

				     (:file "daemon-utils-port" :depends-on ("daemon-share" 
									     #+linux "daemon-utils-linux-port"
									     #-linux (error "Not implemented on none Linux")
									     ))
				     (:file "daemon-core-port" :depends-on ("daemon-share" 
									    "daemon-utils-port"
									     #+linux "daemon-core-linux-port"
									     #-linux (error "Not implemented on none Linux")
									     ))
				     (:file "daemonization" :depends-on ("daemon-share" "daemon-core-port" "daemon-utils-port"))))))
