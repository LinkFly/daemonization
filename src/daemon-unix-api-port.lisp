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

(defmacro recreate-shadow-functions (package functions-with-args &aux (cur-package (load-time-value *package*)))
  `(progn
     ,@(loop for sexpr in (eval functions-with-args)
	  collect (flet ((sym-in-pkg (sym &optional (package cur-package) &aux (sym-name (symbol-name sym)))
			   (find-symbol sym-name package)))
		    (destructuring-bind (fn &rest args)
			sexpr
		      (let ((shadow-fn (sym-in-pkg fn))
			    (shadowed-fn (sym-in-pkg fn package))
			    (args-lambda-list (mapcar #'(lambda (arg)
							  (cond
							    ((member arg '(&optional &rest)) arg)
							    ((consp arg) (list (progn (intern (symbol-name (first arg)) cur-package)
										      (sym-in-pkg (first arg)))
									       (sym-in-pkg (second arg))))
							    ((symbolp arg) 
							     (intern (symbol-name arg) cur-package)
							     (sym-in-pkg arg))))
						      args)))
			(when (null shadow-fn) (call-bad-interface-error fn args-lambda-list cur-package package))
			(when (macro-function shadowed-fn) (error "Can not recreate macro-function ~S" shadowed-fn))
			`(defun-ext ,shadow-fn ,args-lambda-list
			   (apply ',shadowed-fn ,@(loop 
						     with result
						     for arg in (butlast args)
						     do (cond
							  ((member arg '(&optional &rest))) ;do nothing
							  ((consp arg) (push (sym-in-pkg (first arg))
									     result))
							  ((symbolp arg) (push (sym-in-pkg arg) result)))
									       
						     finally (let* ((last-arg (first (last args))))
							       (push (if (null last-arg)
									 nil
									 (case arg
									   (&rest (sym-in-pkg last-arg))
									   (&optional (list 'list 
											    (sym-in-pkg (if (consp last-arg)
													    (first last-arg)
													    last-arg))))
									   (t (list 'list (sym-in-pkg last-arg)))))
								       result)
							       (return (reverse result))))))))))))

;(macroexpand-1 '(recreate-shadow-functions :daemon-sbcl-sys-linux-port (daemon-interfaces:get-unix-functions)))
(defun define-unix-functions ()
  (recreate-shadow-functions 
   #+sbcl
   :daemon-sbcl-sys-linux-port
   #-sbcl 
   #.(error "Not implemented for non sbcl lisp systems")
   (get-unix-functions)))
