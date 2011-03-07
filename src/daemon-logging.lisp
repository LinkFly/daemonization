(defpackage :daemon-logging 
  (:use :cl)
  (:export #:log-info #:log-err #:defun-ext #:wrap-log
	   #:*log-indent* #:*print-log-layer* #:*print-internal-call*))

(in-package :daemon-logging)
;;; Logging for actions on differently layers, for using
;;; defining +log-layer+ (if it not defining then reading name of current package), 
;;; *fn-log-info*, and *fn-log-err* special variables. Example:
(defconstant +log-layer+ :logging-layer)
(defparameter *fn-log-info* #'(lambda (fmt-str &rest args)
				(apply #'format t  fmt-str args)))
(defparameter *fn-log-err #'(lambda (fmt-str &rest args)
				(apply #'format t  fmt-str args)))
;;; Checking
;(log-info "test")
;(syslog-info "test")
;(defun-ext f (x y) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4)
;;;;;;;;;;;;;;;;;

(defparameter *log-indent* 0)
(defparameter *print-log-layer* t)
(defparameter *print-internal-call* t)

;;; For defun-ext and wrap-fmt-str ;;;;;;;;;;;;;;;;;;
(defparameter *def-in-package* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-indent ()
  (coerce (loop repeat *log-indent* collect #\Tab) 'string))

(defun wrap-fmt-str (fmt-str)
  (format nil
	  "~%(:DAEMONIZATION ~A ~A~A)"
	  (if *print-log-layer* 
	      (let ((layer-sym (find-symbol "+LOG-LAYER+" *package*)))
		(if (and layer-sym (boundp layer-sym)) 
		    (format nil "~S" (symbol-value layer-sym))
		    (if *def-in-package*
			(concatenate 'string ":" (package-name *def-in-package*))
			"")))
	      "") 
	  (get-indent)
	  fmt-str))

(defun logging (fn-log-str format-str &rest args)
  (apply (symbol-value (find-symbol fn-log-str *def-in-package*))
	 (wrap-fmt-str format-str)
	 args))

(defmacro syslog-info (format-str &rest args)  
  `(progn
     (logging "*FN-LOG-INFO*" ,format-str ,@args)))

(defmacro log-info (format-str &rest args)
  `(let ((*def-in-package* (load-time-value *package*)))     
     (logging "*FN-LOG-INFO*" (format nil "~S" ,format-str) ,@args)))

(defmacro log-err (format-str &rest args)
  `(let ((*def-in-package* (load-time-value *package*)))     
     (logging "*FN-LOG-ERR*" (format nil "~S" ,format-str) ,@args)))

(defun as-string (sexpr)
  (unless (stringp sexpr)
    (return-from as-string (format nil "~S" sexpr)))
  sexpr)

(defun syslog-call-info (form)
  (syslog-info (format nil ":CALL ~A" (as-string form)))
  (incf *log-indent*))
  
(defun syslog-call-out (result form)
  (if (zerop *log-indent*)
      (error "*log-indent* not must be less zero. Not correct log operation."))
  (decf *log-indent*)
  (syslog-info ":RESULT ~S :CALLED-FORM ~A)" result (as-string form)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(loop for var in vars collect `(,var (gensym)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel)
  (defun remove-declare-ignore (body)
    (flet ((declare-ignore-form-p (form &aux (fst-sym (first form)))
	     (and (eq 'declare fst-sym)
		  (member (first (second form))
			  '(ignore ignorable)))))
      (loop for form in body 
	 when (or (atom form) (not (declare-ignore-form-p form)))
	 collect form)))
  ;(remove-declare-ignore '((declare (ignore x)) (declare (ignorable y)) (+ 3 2) t))    

  (defun present-args (args)
    (flet ((as-keyword (sym)
	     (if (keywordp sym)
		 sym
		 (read-from-string 
		  (concatenate 'string ":" (symbol-name sym))))))
      (loop
	 with cur-arg-type
	 with result
	 for arg in args
	 do (if (and (symbolp arg) (char= #\& (elt (symbol-name arg) 0)))
		(setq cur-arg-type arg)
		(case cur-arg-type
		  (nil (push arg result))
		  (&optional (push arg result))
		  (&key (push (as-keyword arg) result)
			(push arg result))
		  (&rest (push ''&rest result)
			 (push arg result))
		  (&aux nil)))
	 finally (return (reverse result)))))
  ;(present-args '(x y &optional (v 34) &key r m))  
  );eval-when 

(defun correct-sym (sym)
    (format nil 
	    (if (eql (symbol-package sym)
		     (find-package *def-in-package*))
		"~A"
		"~S")
	    sym))
  ;(correct-sym 'daemonization::define-constant)
  #|(let ((*def-in-package* (find-package :daemonization)))
      (correct-sym 'daemonization::define-constant))
  |#

(defun present-form (form)    	
  (if (null form) 
      ""
      (let ((res-str (format nil
			     "(~{~A ~}" 
			     (loop for obj in form
				if (symbolp obj) do (setq obj (correct-sym obj))
				else do (setq obj (format nil "~S" obj))
				end
				collect obj))))
	(concatenate 'string
		     (subseq res-str 0 (1- (length res-str)))
		     ")"))))

#|
  (present-form '(daemonization::define-constant f (x) (* x x)));
  (let ((*def-in-package* (find-package :daemonization)))
     (present-form '(daemonization::define-constant f (x) (* x x))))
  (present-form '(f))
  (present-form nil)
|#

#|(let* ((fst (correct-sym (first form)))
	       (res-str (format nil "(~A ~{~S ~}" fst (rest form))))
	  (concatenate 'string
		       (subseq res-str 0 (1- (length res-str)))
		       ")"))
|#

(defmacro wrap-log-form (form)
  (with-gensyms (form-str)
    `(if (not *print-internal-call*)
	 ,form
	 (let* ((*def-in-package* (load-time-value *package*))
		(*log-indent* *log-indent*)
		(,form-str (present-form ',form)))
	   (syslog-call-info ,form-str)
	   (let ((res ,form))
	     (syslog-call-out res ,form-str)
	     res)))))

(defmacro wrap-log (&rest forms)
  `(progn ,@(loop for form in forms
	       collect `(wrap-log-form ,form))))

(defmacro defun-ext (name args &body body)
  (with-gensyms (form-str)
    `(defun ,name ,args
       (let* ((*def-in-package* (load-time-value *package*))
	     (*log-indent* *log-indent*)
	     (,form-str (present-form (cons ',name ',(present-args args)))))
	 (syslog-call-info ,form-str)
	 (let ((res (progn ,@(remove-declare-ignore body))))
	   (syslog-call-out res ,form-str)
	   res)))))

;(defun-ext f (x y &key z) (log-info "this f") (+ x z (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4 :z 1)
