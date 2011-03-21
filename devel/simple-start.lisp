(require :asdf)
;(setq *load-pathname* #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/devel/simple-start.lisp")
(defun get-daemonization-path ()
  (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*))))

(push (get-daemonization-path) asdf:*central-registry*)
(asdf:load-system :daemonization)

;(defun get-args () (rest *posix-argv*))
(defparameter *args* (rest *posix-argv*))
(defparameter *conf-file* (first *args*))
;(defparameter *conf-file* "daemon.conf")
(defparameter *command* (second *args*))
(defparameter *exit* (when (string-not-equal "nodaemon" *command*) t))
		       

(defparameter *daemon-parameters* 
  (with-open-file (stream 
		   (make-pathname :defaults *load-pathname*
				  :name *conf-file*
				  :type nil
				  :directory (pathname-directory *load-pathname*)))
    (read stream)))

(format t "~%parameters: ~S ~%command: ~S~%" *daemon-parameters* *command*)
(print "daemonization ...")

(apply #'daemonization:daemonized *command* :exit *exit* *daemon-parameters*) 
(print "... end daemonization.")


