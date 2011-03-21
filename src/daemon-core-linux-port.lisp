(defpackage :daemon-core-linux-port
  (:use :cl :daemon-logging :daemon-features :daemon-unix-api :daemon-utils-linux-port)
  (:shadowing-import-from :daemon-unix-api #:open #:close)
  (:import-from :daemon-sys-linux-port 
		;#:*fn-log-info* #:*fn-log-err*
		#:enable-interrupt)
  (:export #:get-daemon-command
	   #:check-daemon-command
	   #:zap-daemon
	   #:stop-daemon
	   #:kill-daemon	   
	   #:start-daemon
	   #:start-as-no-daemon))

(in-package :daemon-core-linux-port)

;;; Checking logging
;(log-info "sdf")
;(defun-ext f (x y &rest r &key z) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4 :z 6)
;;;;;;;;;;;;;;;;;;;

#+daemon.as-daemon
(defun-ext set-global-error-handler ()
  (setf *debugger-hook*
	#'(lambda (condition x)
	    (declare (ignore x))
	    (let ((err (with-output-to-string (out)
			 (let ((*print-escape* nil))
			   (print-object condition out)))))
	      (print err *error-output*)
	      (log-err err))
	    (exit 1))))

#+daemon.as-daemon
(defun-ext unset-global-error-handler ()
  (setf *debugger-hook* nil))
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Daemon commands ;;;;;;;
#+daemon.as-daemon
(progn
  (defun-ext enable-handling-stop-command (daemon-name)
    #-sbcl (error "Not implemented on not sbcl lisps")
    #+sbcl 
    (enable-interrupt sigusr1
		      #'(lambda (sig info context)
			  (declare (ignore sig info context))
			  (progn 
			    (log-info "Stop ~A daemon" (or daemon-name ""))
			    (exit ex-ok)))))

  (defun-ext zap-daemon (pid-file)
    (delete-file pid-file))

  (defun-ext stop-daemon (pid-file)
    (let ((pid (read-pid-file pid-file)))
      (kill pid sigusr1)
      (loop
	 while (ignore-errors (kill pid 0))
	 do (sleep 0.1))
      (delete-file pid-file)))

  (defun-ext kill-daemon (pid-file)
    (kill (read-pid-file pid-file) sigkill)
    (delete-file pid-file))
 
(defun-ext start-daemon (name pid-file &key configure-rights-fn preparation-fn main-fn before-parent-exit-fn)
  (fork-this-process
     :parent-form-before-fork (when configure-rights-fn (funcall configure-rights-fn))
     :parent-form-before-exit before-parent-exit-fn
     :child-form-after-fork (set-global-error-handler)
     :child-form-before-send-success (progn 
					(set-current-dir #P"/")					
					(set-umask 0)
					(when preparation-fn (funcall preparation-fn))
					(enable-handling-stop-command name)					
					(when pid-file (create-pid-file pid-file)))
     :main-child-form (when main-fn (funcall main-fn))))
;     :main-child-form (progn (log-info "calling main-fn ...")))) ;(log-info (with-output-to-string (*standard-output*) (princ 'main-fn))))))

  ) ;feature :daemon.as-daemon

#|(macroexpand-1 '(fork-this-process
     :parent-form-before-fork (when configure-rights-fn (funcall configure-rights-fn))
     :child-form-after-fork (set-global-error-handler)
     :child-form-before-send-success (progn 
				       (set-current-dir #P"/")
				       (set-umask 0)
				       (when preparation-fn (funcall preparation-fn))
				       (enable-handling-stop-command name)
				       (create-pid-file pid-file))
     :main-child-form (when main-fn (funcall main-fn))))
(macroexpand-1 '(WRAP-LOG
     (ENABLE-INTERRUPT SIGUSR1 #'DAEMON-UTILS-LINUX-PORT::SIGNAL-HANDLER)
     (ENABLE-INTERRUPT SIGCHLD #'DAEMON-UTILS-LINUX-PORT::SIGNAL-HANDLER)
     (WHEN CONFIGURE-RIGHTS-FN (FUNCALL CONFIGURE-RIGHTS-FN))))

(macroexpand-1 '(DAEMON-LOGGING::WRAP-LOG-FORM
     (WHEN CONFIGURE-RIGHTS-FN (FUNCALL CONFIGURE-RIGHTS-FN))))
(macroexpand-1 '(WHEN CONFIGURE-RIGHTS-FN (FUNCALL CONFIGURE-RIGHTS-FN)))
|# 

(defun-ext start-as-no-daemon (fn)
  (when fn (funcall fn)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;