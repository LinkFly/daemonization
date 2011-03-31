(require :asdf)

(push (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*)))
      asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)
      (asdf:load-system :daemonization)))

(defparameter *daemon-conf* '(:main-function  nil
			      :pid-file "my-daemon-exp"
			      :exit nil
			      :name nil
			      :user nil
			      :group nil
			      :before-parent-exit-fn nil)) 

(defun daemon-cmd (cmd)
  (daemonization:daemonized cmd *daemon-conf* :on-error :as-ignore-errors))

(defun daemon-status ()
  (daemonization:daemonized "status" *daemon-conf* :on-error :as-ignore-errors))

(defun get-this-path ()
  (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (pathname-directory *load-pathname*)))

(defun get-tests-prompts-file ()
  (make-pathname :defaults (get-this-path)
		 :name "tests-prompts.log"))

(defun get-tests-syslog-file ()
  (make-pathname :defaults (get-this-path)
		 :name "tests-syslog.log"))

(defparameter *tests-syslog-file* (get-tests-syslog-file))
(when (probe-file *tests-syslog-file*) (delete-file *tests-syslog-file*))

(defun print-log (fmt-str &rest args)
  (with-open-file (syslog-stream *tests-syslog-file* 
				 :direction :output :if-does-not-exist :create :if-exists :append)
;    (when (not (open-stream-p syslog-stream))
;      (setq syslog-stream (open *tests-syslog-file* 
;				:direction :output :if-does-not-exist :create :if-exists :append)))
    (apply #'format syslog-stream fmt-str args))) 

(defparameter *parent-pid* (daemonization:getpid))

;(with-open-file (*standard-output* (get-tests-prompts-file)
;			    :direction :output :if-does-not-exist :create :if-exists :supersede)
;(with-open-stream (s1 (open (get-tests-prompts-file) 
;			    :direction :output :if-does-not-exist :create :if-exists :supersede))
;  (with-open-stream (s2 *standard-output*)
;    (with-open-stream (*standard-output* (make-broadcast-stream s1 s2))

(defparameter *log-prompts-stream* (open (get-tests-prompts-file)
					 :direction :output :if-does-not-exist :create :if-exists :supersede))
(defparameter *broadcast-stream* (make-broadcast-stream *log-prompts-stream* *standard-output*))
;(setf *standard-output* (make-broadcast-stream *log-prompts-stream* *standard-output*))
(setf *standard-output* *broadcast-stream*)
			 
(princ "Tests daemonized ... ")  
(terpri)
(block tests 
  (flet ((return-if-child () 
	   (when (/= *parent-pid* (daemonization:getpid))
	     (return-from tests t))))
    (let ((daemon-logging:*fn-log-info* #'print-log)
	  (daemon-logging:*fn-log-err* #'print-log))
      (if (and 
	   (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	   (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	   (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	   (progn (format t "~%try kill ...~%") (daemon-cmd "kill") (not (eql daemon-share:ex-ok (daemon-status))))
	   (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	   (progn (format t "~%try restart ...~%") (daemon-cmd "restart") (return-if-child) (eql daemon-share:ex-ok (daemon-status)))
	   (progn (format t "~%try stop ...~%")(daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	   )
	  (princ " ... Tests passed.")
	  (princ " ... Tests failed.")))
    (terpri)
    ;(close *broadcast-stream*)
    ;(close *log-prompts-stream*)
    (quit)))
;)
;))