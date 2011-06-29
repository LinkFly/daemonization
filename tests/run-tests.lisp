(require :asdf)

(push (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*)))
      asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)
    (asdf:load-system :daemonization-test)))

(defun run-tests (&aux parent-pid)
  (setf parent-pid (daemonization-test:get-proc-id))
  (daemonization-test:run-tests)
  (when (= parent-pid (daemonization-test:get-proc-id))
    (quit)))

(defun root-run-tests (username &aux parent-pid)
  (setf parent-pid (daemonization-test:get-proc-id))
  (when (or (null username) 
	    (not (stringp username))
	    (string= "" username))
    (setf username 
	  (restart-case (error "ERROR: need username")
	    (use-value () 
	      (format *standard-output* "Please enter user name: ")
	      (finish-output)
	      (read-line)))))
  (daemonization-test:root-run-tests username)
  (when (= parent-pid (daemonization-test:get-proc-id))
    (quit)))
    
