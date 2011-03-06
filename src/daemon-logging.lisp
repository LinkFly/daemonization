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

(defun get-indent ()
  (coerce (loop repeat *log-indent* collect #\Tab) 'string))

(defun wrap-fmt-str (fmt-str)
  (format nil
	  "~%(:DAEMONIZATION :~A ~A~A)"
	  (if *print-log-layer* 
	      (let ((layer-sym (find-symbol "+LOG-LAYER+" *package*)))
		(if layer-sym 
		    (symbol-value layer-sym)
		    (package-name *package*)))
	      "") 
	  (get-indent)
	  fmt-str))

(defun logging (fn-log-str format-str &rest args)
  (apply (symbol-value (find-symbol fn-log-str *package*)) 
	 (wrap-fmt-str format-str)
	 args))

(defun syslog-info (format-str &rest args)
  (apply #'logging "*FN-LOG-INFO*" format-str args))

(defun log-info (format-str &rest args)
  (apply #'logging "*FN-LOG-INFO*" (format nil "~S" format-str) args))

(defun log-err (format-str &rest args)
  (apply #'logging "*FN-LOG-ERR*" (format nil "~S" format-str) args))

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

(defmacro wrap-log-form (form)
  (with-gensyms (form-str)
    `(if (not *print-internal-call*)
	 ,form
	 (let ((*log-indent* *log-indent*)
	       (,form-str (princ-to-string ',form)))
	   (syslog-call-info ,form-str)
	   (let ((res ,form))
	     (syslog-call-out res ,form-str)
	     res)))))

(defmacro wrap-log (&rest forms)
  `(progn ,@(loop for form in forms
	       collect `(wrap-log-form ,form))))

(eval-when (:compile-toplevel :load-toplevel)
  (defun remove-declare-ignore (body &aux (fst-form (first body)))
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
		  ((nil &optional) (push arg result))
		  (&key (push (as-keyword arg) result)
			(push arg result))
		  (&rest (push ''&rest result)
			 (push arg result))
		  (&aux nil)))
	 finally (return (reverse result)))))
  ;(present-args '(x y &optional (v 34) &key r m))
  );eval-when


(defmacro defun-ext (name args &body body)
  (with-gensyms (form-str)
    `(defun ,name ,args
       (let ((*log-indent* *log-indent*)
	     (,form-str (format nil "~S" (list ',name ,@(present-args args)))))
	 (syslog-call-info ,form-str)
	 (let ((res (progn ,@(remove-declare-ignore body))))
	   (syslog-call-out res ,form-str)
	   res)))))
