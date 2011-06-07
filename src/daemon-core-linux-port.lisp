(defpackage :daemon-core-linux-port
  (:use :cl :daemon-share :daemon-unix-api-port :daemon-utils-linux-port)
  (:shadowing-import-from :daemon-unix-api-port #:open #:close)
  #+sbcl
  (:import-from :daemon-sbcl-sys-linux-port #:enable-interrupt)
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")  
  (:export #:get-daemon-command
	   #:check-daemon-command
	   #:zap-daemon
	   #:stop-daemon
	   #:kill-daemon	   
	   #:start-daemon
	   #:start-as-no-daemon
	   #:status-daemon))

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
	    (exit 
	     (typecase condition
	       (file-exists-error ex-cantcreate)
	       (t ex-software))))))

#+daemon.as-daemon
#|(defun-ext unset-global-error-handler ()
  (setf *debugger-hook* nil))
|#
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-ext cur-exit (&optional (status ex-ok) extra-status)
  (if *fn-exit*
      (funcall *fn-exit* status extra-status)
      (funcall #'exit status))) 

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
			    (cur-exit ex-ok)))))

  (defun-ext zap-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(progn 
	  (delete-file pid-file)
	  (cur-exit ex-ok (make-extra-status :pid-file pid-file)))))
 
  (defun-ext stop-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(let ((pid (read-pid-file pid-file)))
	  (kill pid sigusr1)
	  (loop
	     while (ignore-errors (kill pid 0))
	     do (sleep 0.1))
	  (delete-file pid-file)
	  (cur-exit ex-ok (make-extra-status :pid pid :pid-file pid-file)))))

  (defun-ext status-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(let ((pid (read-pid-file pid-file)))
	  (apply #'cur-exit 
		 (let ((status (cond 
				 ((ignore-errors (kill pid 0)) ex-ok)
				 (t ex-unavailable))))
		   (list status (make-extra-status :pid pid 
						   :user (when (= ex-ok status)
							   (get-username pid))
						   :pid-file pid-file))))))) 

  (defun-ext kill-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+)
	(let ((pid (read-pid-file pid-file))) 
	  (kill pid sigkill)
	  (delete-file pid-file)
	  (cur-exit ex-ok (make-extra-status :pid pid :pid-file pid-file)))))
 
  (defun-ext start-daemon (name pid-file &key configure-rights-fn preparation-fn main-fn before-parent-exit-fn)
    (fork-this-process
     :fn-exit #'(lambda (&optional (status ex-ok) extra-status)
		  (funcall #'cur-exit
			   status 
			   (with-slots ((es-name name) (es-pid-file pid-file)) extra-status
			     (unless (and (null es-name) (null es-pid-file))
			       (let ((err
"Slots name or/and pid-file of extra-status instance not must be fulled in this point"))
				 (log-err err)
				 (error err)))
			     (setf es-name name
				   es-pid-file pid-file)
			     extra-status)))
     :parent-form-before-fork configure-rights-fn
     :parent-form-before-exit before-parent-exit-fn
     :child-form-after-fork #'set-global-error-handler
     :child-form-before-send-success #'(lambda () 
					 (progn 
					   (set-current-dir #P"/")					
					   (set-umask 0)
					   (when preparation-fn (funcall preparation-fn))
					   (enable-handling-stop-command name)					
					   (when pid-file (create-pid-file pid-file))))
     :main-child-form main-fn))

  ) ;feature :daemon.as-daemon

(defun-ext start-as-no-daemon (fn)
  (when fn (funcall fn)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;