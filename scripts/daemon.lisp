(require :asdf)

(push (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil
		 :directory (butlast (pathname-directory *load-pathname*)))
      asdf:*central-registry*)

(with-output-to-string (*trace-output*)
  (with-output-to-string (*standard-output*)
      (asdf:load-system :daemonization)))

(let* ((args (rest (daemonization:get-args)))
       (conf-file (first args))
       (command (second args)))
  (when (not (eq :absolute (first (pathname-directory conf-file))))
    (setf conf-file 
	  (make-pathname :defaults *load-pathname*
			 :name conf-file
			 :type nil
			 :directory (pathname-directory *load-pathname*))))
  (daemonization:daemonized conf-file command :on-error :exit-from-lisp))
