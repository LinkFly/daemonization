(defun get-daemonization-path ()
  (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*))))

(defmacro with-silence (&body body)
  `(with-output-to-string (*trace-output*)
     (with-output-to-string (*standard-output*)
       (with-output-to-string (*error-output*)
       ,@body))))

(require :asdf)
(push (get-daemonization-path) asdf:*central-registry*)

(with-silence
  (asdf:load-system :daemonization))

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

(funcall #'daemonization:daemonized *command* (append (list :exit *exit*) *daemon-parameters*))


