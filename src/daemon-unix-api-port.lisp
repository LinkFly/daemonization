(daemon-interfaces:define-provider-package
    :daemon-unix-api-port
  #+sbcl
  :daemon-sbcl-sys-linux-port
  #-sbcl 
  #.(error "Not implemented for non sbcl lisp systems")
  (:use (:cl :daemon-share :daemon-interfaces)
   :get-fn-syms-form (daemon-interfaces:get-unix-fn-syms)
   :get-constants-form (daemon-interfaces:get-unix-constants)
   :export (#:define-unix-functions)))

(in-package :daemon-unix-api-port)

(defmacro recreate-shadow-functions (package functions-with-args)
  `(progn
     ,@(loop for sexpr in (eval functions-with-args)
	  collect (flet ((sym-in-pkg (sym &optional (package *package*) &aux (sym-name (symbol-name sym)))
			   (find-symbol sym-name package)))
		    (destructuring-bind (fn &rest args)
			sexpr
		      (let ((shadow-fn (sym-in-pkg fn))
			    (shadowed-fn (sym-in-pkg fn package)))
			`(defun-ext ,shadow-fn ,(mapcar #'(lambda (arg)
							    (cond
							      ((eq '&optional arg) '&optional)
							      ((consp arg) (list (progn (intern (symbol-name (first arg)) *package*)
											(sym-in-pkg (first arg)))
										 (second arg)))
							      ((symbolp arg) 
							       (intern (symbol-name arg) *package*)
							       (sym-in-pkg arg))))
							args)
			   (,shadowed-fn ,@(mapcan #'(lambda (arg)
						       (cond
							 ((eq '&optional arg) nil)
							 ((consp arg) (list (sym-in-pkg (first arg))))
							 ((symbolp arg) (list (sym-in-pkg arg)))))
						   args)))))))))

;(macroexpand-1 '(recreate-shadow-functions :daemon-sbcl-sys-linux-port (daemon-interfaces:get-unix-functions)))
(defun define-unix-functions ()
  (recreate-shadow-functions 
   #+sbcl
   :daemon-sbcl-sys-linux-port
   #-sbcl 
   #.(error "Not implemented for non sbcl lisp systems")
   (daemon-interfaces:get-unix-functions)))
