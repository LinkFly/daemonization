;;;; daemonization.asd
;;;;
;;;; This file is part of the common lisp library - daemonization, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Katrevich Sergey <linkfly1@newmail.ru>

(defsystem :daemonization
  :version "0.0.1"
  :depends-on ()
  :components ((:module "src"
			:components ((:file "daemon-required-features")
				     (:file "daemon-sys-port" :depends-on ("daemon-required-features"))
				     (:file "daemon-utils" :depends-on ("daemon-sys-port"))
				     (:file "daemon-rights-restricting")
				     (:file "daemonization" :depends-on ("daemon-sys-port" "daemon-utils"))))))
				     
			