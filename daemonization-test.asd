(defpackage :daemonization-test-system (:use :cl :asdf))
(in-package :daemonization-test-system)

(defsystem :daemonization-test
  :depends-on (:daemonization)
  :components ((:module "tests"
			:components ((:file "tests")))))

(defconstant +system-name+ :daemonization-test)
(defconstant +daemonization-system+ :daemonization)
(defconstant +test-dir+ (if (boundp '+test-dir+) (symbol-value '+test-dir+) "tests"))

(defparameter *log-filename* "tests-prompts.log")
(defparameter *root-log-filename* "root-tests-prompts.log")
(defparameter *syslog-filename* "tests-syslog.log")
(defparameter *root-syslog-filename* "root-tests-syslog.log")

(defparameter *test-mode* nil)

(defun get-test-mode ()
  (symbol-macrolet ((print-call-p (slot-value (symbol-value (find-symbol "*LOGGER*" :daemon-share))
					      (find-symbol "PRINT-CALL-P" :daemon-share))))
    (let* ((old-val print-call-p)
	   result)
      (setf print-call-p nil
	    result (if (funcall (find-symbol "ADMIN-CURRENT-USER-P"
					     :daemon-utils-port))
		       :root
		       :user)
	    print-call-p old-val)
      result)))

(defun get-system-path ()
  (asdf:component-pathname (asdf:find-system +system-name+)))

(defun get-test-dir ()
  (make-pathname :defaults (get-system-path)
		 :directory (append (pathname-directory (get-system-path)) (list +test-dir+))))

(defun get-full-pathname (file-name)
  (make-pathname :defaults (get-test-dir)
		 :type nil
		 :name file-name))
(setf (symbol-function 'get-full-pathname) #'identity)

(defun get-log-file ()
  (case *test-mode*
    (:user (get-full-pathname *log-filename*))
    (:root (get-full-pathname *root-log-filename*))))

(defun get-syslog-file ()
  (case *test-mode*
    (:user (get-full-pathname *syslog-filename*))
    (:root (get-full-pathname *root-syslog-filename*))))

(defun print-syslog (fmt-str &rest args)
  (with-open-file (syslog-stream (get-syslog-file)
				 :direction :output :if-does-not-exist :create :if-exists :append)
    ;(break "syslog-stream: ~S fmt-str: ~S args: ~S" syslog-stream fmt-str args)
    (apply #'format syslog-stream fmt-str args)))

(defun ensure-no-file (pathname)
  (when (probe-file pathname)    
    (delete-file pathname)))

(defun ensure-no-syslog-file ()
  (ensure-no-file (get-syslog-file)))

(defun recreate-root-syslog-file ()
  (let ((*test-mode* :root)) 
    (funcall (find-symbol "RECREATE-FILE-ALLOW-WRITE-OTHER" :daemon-utils-port) 
	     (get-syslog-file))))

(defun dynvar-sets (pairs)
  (dolist (pair pairs)
    (setf (symbol-value (find-symbol (first pair) :daemon-share)) 
	  (second pair))))

(defmethod perform :before ((operation load-op) 
			    (component (eql (asdf:find-component +daemonization-system+
								 '("src" "daemon-logging-init")))))
  
  
  (let* ((*test-mode* (get-test-mode))
	 (log-file (get-log-file))
	 (syslog-file (get-syslog-file)))
    (setf (symbol-value (find-symbol "*LOGGER*" :daemon-share))
	  (funcall (find-symbol "PLIST-TO-LOGGER" :daemon-share)
		   `(
		     :files-dir ,+test-dir+
		     :admin-files-dir ,+test-dir+
		     :info-destination ,log-file
		     :error-destination ,log-file
		     :trace-destination ,log-file 
		     :admin-info-destination ,syslog-file 
		     :admin-error-destination ,syslog-file
		     :admin-trace-destination ,syslog-file)))))

(defmethod perform :after ((operation load-op) 
			   (component (eql (asdf:find-component +daemonization-system+
								'("src" "daemon-logging-init")))))
  (let* ((*test-mode* (get-test-mode))
	 (fn-wrapped-print-syslog (let ((test-mode-val *test-mode*))
				    #'(lambda (fmt-str &rest args) 
					(let ((*test-mode* test-mode-val))
					  (apply 'print-syslog fmt-str args))))))
    (dynvar-sets `(("*FN-LOG-INFO*" ,fn-wrapped-print-syslog)
		   ("*FN-LOG-ERR*" ,fn-wrapped-print-syslog)
		   ("*FN-LOG-TRACE*" ,fn-wrapped-print-syslog)
		   ("*FN-LOG-INFO-LOAD*" ,fn-wrapped-print-syslog)))
    (ensure-no-syslog-file)
    (case *test-mode*
      (:root (recreate-root-syslog-file)))))
  