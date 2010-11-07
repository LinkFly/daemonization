;;;; daemonization.asd
;;;;
;;;; This file is part of the common lisp library - daemonization, released under Lisp-LGPL.
;;;; Based on restas-daemon.lisp (from contrib RESTAS system).
;;;; See file COPYING for details.
;;;;
;;;; Author: Katrevich Sergey <linkfly1@newmail.ru>

(defsystem :daemonization
  :version "0.0.1"
  :depends-on ()
  :components ((:module "src"
			:components ((:file "daemon-features")
				     #+linux 
				     (:file "daemon-sys-linux-port" :depends-on ("daemon-features"))
				     #+linux 
				     (:file "daemon-utils-linux-port" :depends-on ("daemon-features" "daemon-sys-linux-port"))
				     #+linux 
				     (:file "daemon-core-linux-port" :depends-on ("daemon-features" 
										  "daemon-sys-linux-port" 
										  "daemon-utils-linux-port"))
				     (:file "daemon-utils-port" :depends-on ("daemon-features"
									     #+linux "daemon-utils-linux-port"
									     #-linux (error "Not implemented on not Linux")
									     ))
				     (:file "daemon-core-port" :depends-on ("daemon-features"
									    "daemon-utils-port"
									     #+linux "daemon-core-linux-port"
									     #-linux (error "Not implemented on not Linux")
									     ))
				     (:file "daemonization" :depends-on ("daemon-core-port"))))))
				     
			