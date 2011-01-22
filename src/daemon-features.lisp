(defpackage :daemon-features
  (:use :cl))

(in-package :daemon-features)

(eval-when (:compile-toplevel)
  (defparameter *enabled-features* 
    ;; Configurate it    
    "enabled-features.sexp"
    #|
    ;; Default:
    '(:daemon.change-user t
      :daemon.listen-privileged-ports t
      :daemon.as-daemon t)
    |#
    "Configurate it. Must be string with filename contained list parameters (then file must be in directory contain this file). 
Or PATHNAME contained full path to file contained list parameters. Or itself list parameters. List parameters 
must be like it: 
(:daemon.change-user t
 :daemon.listen-privileged-ports t		     
 :daemon.as-daemon t)
These parameters included as features into lisp systems.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-sexpr-from-file (file)
  (with-open-file (s file)
    (read s)))

(defun get-config-parameters (enabled-features)  
;(setq enabled-features *enabled-features*)
  "Used get-config-pathnames function."
  (typecase enabled-features
    (string (read-sexpr-from-file (make-pathname :defaults *compile-file-truename*
						 :name enabled-features
						 :type nil)))
    (pathname (read-sexpr-from-file enabled-features))
    (list enabled-features)))

  (loop for (feature enabled-p . nil) on (get-config-parameters *enabled-features*) by #'cddr
     when enabled-p do (pushnew feature *features*)))

