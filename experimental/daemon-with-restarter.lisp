(require :asdf)

(defun get-daemonization-path ()
  (truename 
   (make-pathname :defaults *load-pathname*
		  :name nil
		  :type nil
		  :directory (butlast (pathname-directory *load-pathname*)))))

(defun get-restart-process-path ()
  (truename 
   (make-pathname :defaults *load-pathname*
		  :name nil
		  :type nil
		  :directory (append (pathname-directory *load-pathname*) '("restart-process")))))

(push (get-daemonization-path) asdf:*central-registry*)
(push (get-restart-process-path) asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)
    (with-output-to-string (*error-output*)
      (asdf:load-system :daemonization)
      (asdf:load-system :restart-process))))

(daemonization:daemonized (second (daemonization:get-args))
			  (list 
			   :pid-file nil ;for correct restarting
			   :main-function #'(lambda () 
					      ;; Here your initilization code, as example - loading start swank server script
					      ;(load "/media/WORK_PARTITION/work_dir/web-projects/development/lisp/start-swank.lisp")
					      ) 
			   :exit t
			   :name nil
			   :user nil
			   :group nil
			   :before-parent-exit-fn #'(lambda (pid status)			       
						      (unless (= status sb-posix:sigchld)
							(restart-process:restart-process-on-stops pid
												  :revision-timeout 3)))))


