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
  (setf parent-pid (daemonization-test:getpid))
  (daemonization-test:run-tests)
  (when (= parent-pid (daemonization-test:getpid))
    (quit)))

(defun root-run-tests (username &aux parent-pid)
  (setf parent-pid (daemonization-test:getpid))
  (when (string= "" username)
    (error "ERROR: need username"))
  (daemonization-test:root-run-tests username)
  (when (= parent-pid (daemonization-test:getpid))
    (quit)))
    
