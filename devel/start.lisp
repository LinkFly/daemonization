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

(asdf:load-system :sb-posix)
(asdf:load-system :daemonization)
(asdf:load-system :restart-process)

(daemonization:daemonized "start" 
;    :user "lispuser" 
    :pid-file "/home/lispuser/tmp/pid" 
    :main-function #'(lambda () 
		       (load "/media/WORK_PARTITION/work_dir/web-projects/development/lisp/start-swank.lisp"))
    :before-parent-exit-fn #'(lambda (pid status)			       
			       (unless (= status sb-posix:sigchld)
				 (restart-process:restart-process-on-stops pid
									   :revision-timeout 3)))
    :exit t)


