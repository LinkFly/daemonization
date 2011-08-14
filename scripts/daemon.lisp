(require :asdf)

(push (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*)))
      asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)    
    (asdf:load-system :daemonization)))
    
(defparameter +help-message+ 
  (format nil "Usage: daemon.sh <config-file> <command>
Options:
<config-file> - file with a property list that can contain these keys: ~S
<command> - one of the commands: ~{~A | ~}help" 
	  daemonization:+conf-parameters+
	  daemonization:+all-daemon-commands+))

(defun print-help-message ()
  (format t "~&~A~%" +help-message+))

(let* ((args (rest (daemonization-utils:get-args)))
       (conf-file (first args))
       (command (second args)))  
  (if (or (string= command "help")
	  (not (member command daemonization:+all-daemon-commands+ :test #'string=)))      
      (progn (print-help-message) (quit))
      (progn 
	(when (not (eq :absolute (first (pathname-directory conf-file))))
	  (setf conf-file 
		(make-pathname :defaults *load-pathname*
			       :name conf-file
			       :type nil
			       :directory (pathname-directory *load-pathname*))))
	(daemonization:daemonized conf-file command :on-error :exit-from-lisp))))


