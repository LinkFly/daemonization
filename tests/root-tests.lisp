(require :asdf)

(push (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*)))
      asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)
      (asdf:load-system :daemonization)))
(defparameter *daemon-user* (second (daemonization:get-args)) "Setting when start run script")
(unless *daemon-user*
  (error "Need username argument!"))

(defparameter *daemon-conf* `(:main-function  nil
			      :pid-file "my-daemon-exp"
			      :exit nil
			      :name nil
			      :user ,*daemon-user*
			      :group nil
			      :before-parent-exit-fn nil)) 

(defun daemon-cmd (cmd)
  (daemonization:daemonized cmd *daemon-conf* :on-error :as-ignore-errors))

(defun daemon-status (&aux val-list)
  (daemonization:daemonized "status" *daemon-conf* :on-error :as-ignore-errors))

(princ "Tests daemonized ... ")
(defparameter *parent-pid* (daemonization:getpid))
(terpri)
(block tests 
  (flet ((return-if-child () 
	   (when (/= *parent-pid* (daemonization:getpid))
	     (return-from tests t))))
    (format t "~%--- WITH DO TRY CHANGE USER --~%")
    (if (and 
	 (progn (format t "~%try start ...~%") (daemon-cmd "start") (return-if-child) (equal (list daemon-share:ex-ok *daemon-user*)
											     (let ((multiple-res (multiple-value-list (daemon-status))))
											       (list (first multiple-res)
												     (getf (second multiple-res) :user)))))
	 (progn (format t "~%try stop ...~%") (daemon-cmd "stop") (not (eql daemon-share:ex-ok (daemon-status))))
	 )
	(princ " ... Tests passed.")
	(princ " ... Tests failed."))
    (terpri)
    (quit)))