(defpackage :restart-process 
  (:use :cl :split-sequence ;:trivial-shell
	:sb-ext)
  (:import-from :sb-posix #:getpid)
  (:export #:restart-process-on-stops))

(in-package :restart-process)

(defun getcmd (pid) 
  (with-open-file (stream (format nil "/proc/~A/cmdline" pid))
    (read-line stream)))

(defun get-program-and-args (pid)
  ;(setq pid (sb-posix:getpid))
  ;(setq cmd (getcmd pid))
  ;(setq endpos-program (search (string #\Nul) cmd))
  ;(setq cmd-args (subseq cmd endpos-program))
  ;(setq program (subseq cmd 0 endpos-program))
  (let* (
	 (cmd (getcmd pid))
	 (endpos-program (search (string #\Nul) cmd))
	 (cmd-args (subseq cmd endpos-program))
	 (program (subseq cmd 0 endpos-program)))
    (list program (split-sequence:split-sequence #\Nul (string-trim (string #\Nul) cmd-args)))))

(defun get-start-time (pid)
  ;(trivial-shell:shell-command (format nil "stat -c %x /proc/~A" pid))
  ;(sb-posix:stat-ctime  (sb-posix:stat (format nil "/proc/~A" pid)))
  (file-write-date (format nil "/proc/~A" pid)))

(defun zombie-process-p (pid)
  (with-open-file (stream (format nil "/proc/~A/status" pid))
    (read-line stream)
    (destructuring-bind (param val)
	(split-sequence:split-sequence #\Tab 
				       (read-line stream))
      (when (string-equal "State:" param)
	(char-equal #\Z (elt val 0))))))

(defun probe-pid-p (pid &optional start-time-str)  
  (let ((is-file (and (probe-file (format nil "/proc/~A" pid))
		      (not (zombie-process-p pid)))))
    (unless is-file (return-from probe-pid-p nil))
    (if (not start-time-str)
	t 
	(equalp start-time-str
	       (get-start-time pid)))))

(defun start-program (program-and-args)
  (format t "(:pid ~A) call: (start-program ...)" (getpid))
  (format t "(:pid ~A) result: ~S~%" (getpid)
  (apply #'sb-ext:run-program 
	 (append program-and-args
		 (list :output nil
		       :input nil 
		       :search t
		       :wait nil)))
  )
  (format t "(:pid ~A)... after run-program.~%" (getpid)))

(defparameter *need-stop-revisor* nil)
(defun restart-process-on-stops (pid &key (revision-timeout 3) &aux program-and-args start-time)
;(setq wait-rev-sec 3)
;(setq wait-start-sec 60)
  (setq start-time (get-start-time pid))
  (setq program-and-args (get-program-and-args pid))
  (loop
     while (not *need-stop-revisor*)
     do (format t "~&this pid = ~A. Try (probe-pid-p ~A) ...~%" (getpid) pid)
     do (if (probe-pid-p pid start-time) 	    
	    (progn 
	      (format t "~&... (:pid ~A) process(:pid ~A) is life.~%" (getpid) pid)
	      (sleep revision-timeout))
	      (progn 
		(format t "~&... (:pid ~A) process(:pid ~A) is not life! Restarting ...~%" (getpid) pid)
		(start-program program-and-args)
		;(format t "... (this pid = ~A) clone starting with cmd: ~S ..." (getpid) program-and-args)
		;(start-program '("sbcl" nil))
		(return) 
		;(sleep wait-start-sec)
		)))
  (format t "~&(:pid ~A) Exit revision~%" (getpid)))

;(restart-process-on-stops 16181)
;(defparameter *need-stop-revisor* nil)

;(trivial-shell:shell-command "stat /proc/self")
;(sb-ext:run-program "ps " nil :output *standard-output* :search t)
       

