(defpackage :daemon-share-port
  (:use :cl :daemon-share)
  (:export #:def-alien-call
	   #:*cap-from-text* #:*cap-set-proc* #:*cap-free*
	   #:set-cap-functions
	   #:*libcap-probable-libs*
	   #:*finded-libcap*))

(in-package :daemon-share-port)

#+sbcl
(defmacro def-alien-call (name &rest args &aux fn-str-name)
  (setq fn-str-name (string-upcase
		     (substitute #\- #\_ name)))
  `(progn
     (log-info-load "try defining ~A ..." ,name)
     (sb-posix::define-call ,name ,@args)
     (let ((fn-sym (find-symbol (string-upcase ,fn-str-name) :sb-posix))
	   (fn-using-sym (read-from-string ,fn-str-name)))
       (setf (symbol-function fn-using-sym) (symbol-function fn-sym))
       (log-info-load " ... OK. (symbol-function '~S) => ~S"
	       fn-using-sym (symbol-function fn-using-sym)))))

#+(and daemon.listen-privileged-ports linux)
(progn
  (defparameter *libcap-probable-libs* '("/lib/libcap.so.2" "/lib/libcap.so" "/lib/x86_64-linux-gnu/libcap.so.2" "/lib/i386-linux-gnu/libcap.so.2"))
  (defparameter *finded-libcap* nil)

  (defparameter *cap-from-text* nil)
  (defparameter *cap-set-proc* nil)
  (defparameter *cap-free* nil)

  (defun-ext set-cap-functions (cap-from-text cap-set-proc cap-free)
    (defparameter *cap-from-text* cap-from-text)
    (defparameter *cap-set-proc* cap-from-text)
    (defparameter *cap-free* cap-from-text))

  ) ;progn
