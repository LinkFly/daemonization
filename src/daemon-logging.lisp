(defpackage :daemon-logging 
  (:use :cl)
  (:export #:log-info #:log-err #:defun-ext #:wrap-log
	   #:*print-log-info* #:*print-log-err*
	   #:*log-indent* #:*print-log-layer* #:*print-internal-call* 
	   #:*print-call #:*print-called-form-with-result*))

(in-package :daemon-logging)

;;; Logging configure parameters
(defparameter *log-indent* 0)
(defparameter *log-indent-size* 2)
(defparameter *print-log-info* t)
(defparameter *print-log-err* t)
(defparameter *print-log-layer* t)
(defparameter *print-internal-call* t)
(defparameter *print-call* t)
(defparameter *print-called-form-with-result* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Logging for actions on differently layers, for using
;;; defining +log-layer+ (if it not defining then reading name of current package), 
;;; *fn-log-info*, and *fn-log-err* special variables. Example:

(defconstant +log-layer+ :logging-layer)
(defparameter *fn-log-info* #'(lambda (fmt-str &rest args)
				(apply #'format t fmt-str args)))
(defparameter *fn-log-err* #'(lambda (fmt-str &rest args)
				(apply #'format t fmt-str args)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For defun-ext and wrap-fmt-str ;;;;;;;;;;;;;;;;;;
(defparameter *def-in-package* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-indent ()
  (make-string *log-indent* :initial-element #\Space))

(defun get-log-fn (fn-log-str)
  (let ((sym (when *def-in-package* (find-symbol fn-log-str *def-in-package*))))
    (if (and sym (boundp sym))
	(symbol-value sym)
	(symbol-value (find-symbol fn-log-str *package*)))))

(defun get-fn-log-info ()
  (get-log-fn (symbol-name '*FN-LOG-INFO*)))
(defun get-fn-log-err ()
  (get-log-fn (symbol-name '*FN-LOG-ERR*)))

(defun get-log-layer ()
  (let ((layer-sym (when *def-in-package* (find-symbol (symbol-name '+LOG-LAYER+) *def-in-package*))))
    (if (and layer-sym (boundp layer-sym)) 
	(format nil "~S" (symbol-value layer-sym))
	(concatenate 'string ":" (package-name (or *def-in-package* *package*))))))


;;; Checking
;(get-log-layer)
;(log-info "test")
;(syslog-info "test")
;(defun-ext f (x y) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4)
;;;;;;;;;;;;;;;;;

(defun wrap-fmt-str (fmt-str)
  (format nil
	  "~%(:DAEMONIZATION~A ~A~A)"
	  (if *print-log-layer* 
	      (format nil " ~A" (get-log-layer))
	      "")
	  (get-indent)
	  fmt-str))

(defun logging (fn-log format-str &rest args)
  (apply fn-log (wrap-fmt-str format-str) args))

(defun syslog-info (format-str &rest args)  
  (apply #'logging (get-fn-log-info) format-str args))

(defmacro log-info (format-str &rest args)
  `(when *print-log-info* 
     (let ((*def-in-package* (load-time-value *package*)))     
       (logging (get-fn-log-info) (format nil "~S" ,format-str) ,@args))))

(defmacro log-err (format-str &rest args)
  `(when *print-log-err*
     (let ((*def-in-package* (load-time-value *package*)))     
       (logging (get-fn-log-err) (format nil "~S" ,format-str) ,@args))))

(defun as-string (sexpr)
  (unless (stringp sexpr)
    (return-from as-string (format nil "~S" sexpr)))
  sexpr)

(defun syslog-call-info (form)
  (syslog-info (format nil ":CALL ~A" (as-string form)))
  (incf *log-indent* *log-indent-size*))
  
(defun syslog-call-out (result &optional form)  
  (decf *log-indent* *log-indent-size*)
  (if (< *log-indent* 0)
      (error "*log-indent* not must be less zero. Not correct log operation."))
  (syslog-info ":RESULT ~S ~A"
	       result 
	       (if form 
		   (format nil ":CALLED-FORM ~A)" (as-string form))
		   "")))
  
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
		(if (null cur-arg-type)
		    (push arg result)
		    (case cur-arg-type
		      (&optional (push (if (atom arg) 
					   arg
					   (first arg))
				     result))
		      (&key (let ((arg (if (atom arg) 
					   arg
					   (first arg))))
			      (push (as-keyword arg) result)
			      (push arg result)))
		      (&rest (push ''&rest result)
			     (push arg result))
		      (&aux nil))))
	 finally (return (reverse result)))))
  ;(present-args '("no-daemon"))
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

(defun present-function (function)  
  (third (multiple-value-list (function-lambda-expression function))))


#|(defun represent-functions (fn-list)
  (mapcar #'(lambda (sexp)
	      (if (typep sexp 'function)
		  (present-function sexp)
		  sexp))
	  fn-list))
|#

(defun str-list-close (str)
  (concatenate 'string
	       (subseq str 0 (1- (length str)))
	       ")"))

(defun present-form (form)  
  (cond 
    ((consp form)  (if (and (= 2 (length form)) 
			    (eq 'quote (first form)))
		       (format nil "'~A" (present-form (second form)))
		       (str-list-close 
			(format nil "(~{~A ~}" (mapcar #'present-form form)))))
    ((symbolp form) (correct-sym form))
    ((functionp form) (format nil "~S" (present-function form)))    
    (t (format nil "~S" form))))

(defun present-form (form)    	
  (if (null form) 
      ""
      (let ((res-str (format nil
			     "(~{~A ~}" 
			     (loop for obj in form
				collect (cond 
					  ((symbolp obj) (correct-sym obj))
					  ((functionp obj) (format nil "~S" (present-function obj)))
					  (t (format nil "~S" obj)))))))
					    
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
  (with-gensyms (form-str res fn args)
    `(if (not *print-internal-call*)
	 ,form
	 (let* ((*def-in-package* (load-time-value *package*))
		(*log-indent* *log-indent*)
		(,fn ',(first form))
		(,args ,(cons 'list (rest form)))
		(,form-str (present-form (cons ,fn ,args))))
	   (syslog-call-info ,form-str)
	   (let ((,res (apply ,fn ,args)))
	     (syslog-call-out ,res (when *print-called-form-with-result* ,form-str))
	     ,res)))))

(defmacro wrap-log (&rest forms)
  `(progn ,@(loop for form in forms
	       collect `(wrap-log-form ,form))))

(defmacro defun-ext (name args &body body)
  (with-gensyms (form-str res)
    `(defun ,name ,args
       (let* ((*def-in-package* (load-time-value *package*))
	     (*log-indent* *log-indent*)
	     (,form-str (present-form (cons ',name 
					    (mapcar #'(lambda (arg)
							(if (consp arg)
							    (list 'quote arg)
							    arg))
						    (list ,@(present-args args)))))))
	 (when *print-call* (syslog-call-info ,form-str))
;	 (let ((res (progn ,@(remove-declare-ignore body))))
;	 (let ((,res (locally ,@body)))
	 (let ((,res (locally ,@(remove-declare-ignore body))))
	   (when *print-call* 
	     (apply #'syslog-call-out ,res (when *print-called-form-with-result* (list ,form-str))))
	   ,res)))))

;(defun-ext f (x y &key z) (log-info "this f") (+ x z (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4 :z 1)
