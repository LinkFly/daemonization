(defpackage :daemon-logging-init 
  (:use :cl :daemon-share :daemon-logging :daemon-utils-port :daemon-core-port))

(in-package :daemon-logging-init)

(defun gen-fn-log (type fn-system-log)
  (declare (type (or (eql :info) (eql :error) (eql :trace)) type)
	   (type function fn-system-log))
  (lambda (fmt-str &rest args &aux
	   file-stream-system
	   (is-admin (let ((*print-call* nil)) (admin-current-user-p)))
	   (logger *logger*))
    (flet ((get-file-stream-system (getters-plist)
	     (funcall (funcall (if is-admin #'first #'second)
			       (getf getters-plist type))
		      logger))
	   (get-file-dir (admin-logs-dir-getter logs-dir-getter) 
	     (pathname-as-directory
	      (get-real-file (funcall (if is-admin admin-logs-dir-getter logs-dir-getter)
				      logger)))))
			     
      (setf file-stream-system (get-file-stream-system
				'(:info (logger-admin-info-destination logger-info-destination)
				  :error (logger-admin-error-destination logger-error-destination)
				  :trace (logger-admin-trace-destination logger-trace-destination))))

      (when (typep file-stream-system '(or string pathname))
	(setf file-stream-system (get-real-file file-stream-system
						(get-file-dir 'logger-admin-files-dir 'logger-files-dir))))      
      (typecase file-stream-system
	((eql :system) (apply fn-system-log fmt-str args))
	((or pathname string stream) 	 
	 (apply #'safe-write file-stream-system fmt-str args)
	 (force-output))))))

(defun is-property-p (prop plist)
  (loop for (key . rest) on plist by #'cddr
     when (eq prop key) do (return t)))

(defun get-datetime ()
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month date hour min sec)))

(defun logging-init ()  
  (setf *logger* (if *logger* 
		     *logger*
		     (plist-to-logger (with-open-file (stream (get-logging-conf-file))
					(read stream))))
	*fn-log-info* #'(lambda (fmt-str &rest args)
			  (add-daemon-log (apply #'format nil fmt-str args))
			  (apply (gen-fn-log :info #'syslog-info) fmt-str args))
	*fn-log-info-load* *fn-log-info*
	*fn-log-err* #'(lambda (fmt-str &rest args)
			 (add-daemon-log (concatenate 'string "ERROR: " (apply #'format nil fmt-str args)))
			 (apply (gen-fn-log :error #'syslog-err) (concatenate 'string "ERROR: " fmt-str) args))
	*fn-log-trace* #'(lambda (fmt-str)
			   (apply (gen-fn-log :trace #'syslog-info) "~A" (add-daemon-log fmt-str) nil))
	*fn-log-pid* #'(lambda () (let ((*print-call* nil)) (getpid)))
	*fn-create-log-plist* (lambda (fmt-str &key extra-fmt-str (indent ""))
				(declare (ignore indent))
				(create-log-plist 
				 (:daemonization *log-mode*)
				 (:line *log-line-number* *print-log-line-number*)
				 (:message fmt-str (member *log-mode* '(:info :error)))
				 (:call fmt-str (and (eq *log-mode* :trace) (eq *trace-type* :call) *print-call*))
				 (:result fmt-str (and (eq *log-mode* :trace) (eq *trace-type* :result) *print-call*))
				 (:called-form extra-fmt-str (and (eq *log-mode* :trace)
								  (eq *trace-type* :result)
								  *print-call*
								  *print-called-form-with-result*))
				 (:datetime (get-datetime) *print-log-datetime*)
				 (:pid (funcall *fn-log-pid*) *print-pid*)
				 (:layer (get-log-layer) *print-log-layer*)
				 (:trace-fn *trace-fn*)
				 (:type-proc *process-type*)
				 (:user-name (funcall *fn-get-username*) *print-username*)
				 (:group-name (funcall *fn-get-groupname*) *print-groupname*)))
	*fn-correct-log-plist* #'(lambda (log-plist)
				   (when (getf log-plist :line)
				     (labels ((is-log-trace? () 
						(eq :trace (getf log-plist :daemonization)))
					      (is-daemonized-result? ()
						(and (is-log-trace?) 
						     (getf log-plist :result)
						     (eq *main-function-symbol* (getf log-plist :trace-fn)))))
				       (symbol-macrolet ((count-ls (logger-count *logger*)))					 
					 (setf (getf log-plist :line) (copy-list count-ls))
					 (incf (second count-ls))
					 (when (is-daemonized-result?)
					   (setf (first count-ls) (incf (first count-ls)))
					   (setf (second count-ls) 1))
					 log-plist))))
	*fn-wrapped-begin-fmt-str* (lambda (log-plist &optional (indent ""))	   
				     (setf log-plist (copy-list log-plist))
				     (let ((message (getf log-plist :message))
					   (line-number (getf log-plist :line))
					   (cur-main-key (loop for key in '(:message :call :result)
							    if (is-property-p key log-plist) do (return key))))
				       (values
					(concatenate 'string 
						     (prog1 (format nil "~S ~6S" (first log-plist) (second log-plist))
						       (remf log-plist (first log-plist)))
						     (concatenate 'string 
								  (if (is-property-p :line log-plist)
								      (format nil " ~S ~9A " :line line-number)
								      " ")
								  (format nil "~8S "cur-main-key)
								  indent
								  (let ((main-value (getf log-plist cur-main-key)))
								    (cond 
								      ((eq :message cur-main-key) 
								       (concatenate 'string " \"" message "\""))
								      ((member cur-main-key '(:call :result))
								       main-value)))))
					(loop :for key :in '(:line :message :call :result) 
					   :do (remf log-plist key)
					   :finally (return log-plist)))))
	*fn-print-pair* (lambda (pair)
			  (if (eq (first pair) :called-form)
			      (format nil " ~S ~A" (first pair) (second pair))
			      (format nil " ~S ~S" (first pair) (second pair))))
	
	*fn-get-username* (lambda () (let ((*print-call* nil)) (get-username)))
	*fn-get-groupname* (lambda () (let ((*print-call* nil)) (get-groupname)))))

(logging-init)

  