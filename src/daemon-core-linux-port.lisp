(defpackage :daemon-core-linux-port
  (:use :cl :daemon-share :daemon-unix-api-port :daemon-utils-linux-port)
  (:shadowing-import-from :daemon-unix-api-port #:open #:close)
  #+sbcl
  (:import-from :daemon-sbcl-sys-linux-port #:enable-interrupt #:import-sys-functions-and-constants 
		#:log-info-constant #:log-err-constant
		#:get-error-description)
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")  
  (:export #:get-daemon-command
	   #:check-daemon-command
	   #:zap-daemon
	   #:stop-daemon
	   #:kill-daemon	   
	   #:start-daemon
	   #:start-as-no-daemon
	   #:status-daemon

	   #:syslog-info #:syslog-err
	   #:import-sys-functions-and-constants
	   #:define-unix-functions

	   #:get-error-description))

(in-package :daemon-core-linux-port)

;;; Checking logging
;(log-info "sdf")
;(defun-ext f (x y &rest r &key z) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4 :z 6)
;;;;;;;;;;;;;;;;;;;

(defun syslog-info (fmt-str &rest args)
  (let ((*print-call* nil))
    (apply 'syslog log-info-constant fmt-str args)))

(defun syslog-err (fmt-str &rest args)
  (let ((*print-call* nil))
    (apply 'syslog log-err-constant fmt-str args)))

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
	       (file-exists-error +ex-cantcreate+)
	       (t +ex-software+))))))

#+daemon.as-daemon
#|(defun-ext unset-global-error-handler ()
  (setf *debugger-hook* nil))
|#
;;;;;;;;;;;;;;;;;;;;;;;;

(defun-ext cur-exit (&optional (status +ex-ok+) extra-status)
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
		      (let ((fn-log-info-val *fn-log-info*)
			    (fn-log-trace-val *fn-log-trace*)) 
			#'(lambda (sig info context)
			    (declare (ignore sig info context))
			    (let ((*fn-log-info* fn-log-info-val)
				  (*fn-log-trace* fn-log-trace-val))
			      (progn
				(log-info "Stop~A daemon" (if daemon-name (concatenate 'string " " daemon-name) ""))
				(cur-exit +ex-ok+)))))))

  (defun-ext zap-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(progn 
	  (delete-file pid-file)
	  (cur-exit +ex-ok+ (make-extra-status :pid-file pid-file)))))
 
  (defun-ext stop-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(let* ((pid (read-pid-file pid-file))
	       (extra-status (make-extra-status :pid pid :pid-file pid-file)))
	  (unless (ignore-errors (kill pid 0))
	    (cur-exit +process-not-exists+ extra-status))
	  (kill pid sigusr1)
	  (loop
	     while (ignore-errors (kill pid 0))
	     do (sleep 0.1))
	  (delete-file pid-file)
	  (cur-exit +ex-ok+ extra-status))))

  (defun-ext status-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+ (make-extra-status :pid-file pid-file))
	(let ((pid (read-pid-file pid-file)))
	  (apply #'cur-exit 
		 (let ((status (cond 
				 ((ignore-errors (kill pid 0)) +ex-ok+)
				 (t +ex-unavailable+))))
		   (list status (make-extra-status :pid pid 
						   :user (when (= +ex-ok+ status)
							   (get-username))
						   :pid-file pid-file))))))) 
  
  (defun-ext kill-daemon (pid-file)
    (if (not (probe-file pid-file))
	(cur-exit +pid-file-not-found+)
	(let ((pid (read-pid-file pid-file))) 
	  (kill pid sigkill)
	  (delete-file pid-file)
	  (cur-exit +ex-ok+ (make-extra-status :pid pid :pid-file pid-file)))))
 
  (defun-ext start-daemon (name pid-file 
			   &key configure-rights-fn preparation-fn before-init-fn main-fn before-parent-exit-fn os-params)
    (fork-this-process
     :fn-exit #'(lambda (&optional (status +ex-ok+) extra-status)
		  (funcall #'cur-exit
			   status 
			   (let ((es-name (extra-status-name extra-status)) 
				 (es-pid-file (extra-status-pid-file extra-status)))
			     (unless (and (null es-name) (null es-pid-file))
			       (let ((err
"Slots name or/and pid-file of extra-status instance not must be fulled in this point"))
				 (log-err err)
				 (error err)))
			     (setf (extra-status-name extra-status) name
				   (extra-status-pid-file extra-status) pid-file)
			     extra-status)))
     :parent-form-before-fork configure-rights-fn
     :parent-form-before-exit before-parent-exit-fn
     :child-form-after-fork #'(lambda ()
				(when before-init-fn (funcall before-init-fn))
				(set-global-error-handler))
     :child-form-before-send-success #'(lambda () 
					 (progn 
					   (set-current-dir #P"/")			
					   (set-umask (let (umask) 
							(if (setf umask (getf os-params :linux-umask))
							    umask
							    0)))
					   (when preparation-fn (funcall preparation-fn))
					   (enable-handling-stop-command name)		
					   (when pid-file (create-pid-file pid-file))))
     :main-child-form main-fn))

  ) ;feature :daemon.as-daemon

(defun-ext start-as-no-daemon (fn)
  (when fn (funcall fn)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;