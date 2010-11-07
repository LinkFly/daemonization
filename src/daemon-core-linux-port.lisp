(defpackage :daemon-core-linux-port
  (:use :cl :daemon-features :daemon-sys-linux-port :daemon-utils-linux-port)
  (:shadowing-import-from :daemon-sys-linux-port #:open)
  (:export #:get-daemon-command
	   #:check-daemon-command
	   #:zap-daemon
	   #:stop-daemon
	   #:kill-daemon	   
	   #:start-daemon
	   #:start-as-no-daemon))

(in-package :daemon-core-linux-port)


#+daemon.as-daemon
(defun set-global-error-handler ()
  (setf *debugger-hook*
	#'(lambda (condition x)
	    (declare (ignore x))
	    (let ((err (with-output-to-string (out)
			 (let ((*print-escape* nil))
			   (print-object condition out)))))
	      (print err *error-output*)
	      (syslog-error err))
	    (exit 1))))

#+daemon.as-daemon
(defun unset-global-error-handler ()
  (setf *debugger-hook* nil))
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Daemon commands ;;;;;;;
#+daemon.as-daemon
(progn
  (defun enable-handling-stop-command (daemon-name)
    #-sbcl (error "Not implemented on not sbcl lisps")
    #+sbcl 
    (enable-interrupt sigusr1
		      #'(lambda ()
			  (handler-case 
			      (progn 
				(syslog-info "Stop ~A daemon" daemon-name)
				(error "~A stop" daemon-name))
			    (error (err)
			      (syslog-error (with-output-to-string (out)
					      (let ((*print-escape* nil))
						(print-object err out))))))
			    (exit ex-ok))))

  (defun zap-daemon (pid-file)
    (delete-file pid-file)
    (exit ex-ok))

  (defun stop-daemon (pid-file)
    (let ((pid (read-pid-file pid-file)))
      (kill pid sigusr1)
      (loop
	 while (ignore-errors (kill pid 0))
	 do (sleep 0.1))
      (exit ex-ok)))

  (defun kill-daemon (pid-file)
    (kill (read-pid-file pid-file) sigkill)
    (delete-file pid-file)
    (exit ex-ok))

  (defun start-daemon (name &key configure-rights-fn preparation-fn main-fn)
    (fork-this-process 
     :parent-form-before-fork (when configure-rights-fn (funcall configure-rights-fn))
     :child-form-after-fork (set-global-error-handler)
     :child-form-before-send-success (progn 
				       (set-current-dir #P"/")
				       (set-umask 0)
				       (when preparation-fn (funcall preparation-fn))
				       (enable-handling-stop-command name)
				       (create-pid-file pid-file))
     :main-child-form (when main-fn (funcall main-fn))))

  ) ;feature :daemon.as-daemon

(defun start-as-no-daemon (fn)
  (funcall fn))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;