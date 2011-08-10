(defpackage :daemon-logging 
  (:use :cl)
  (:export #:log-info #:log-err #:defun-ext #:wrap-log
	   #:*print-log-info* #:*print-log-err*
	   #:*log-indent* #:*print-log-layer* #:*print-internal-call* 
	   #:*print-call* #:*print-called-form-with-result*
	   #:*print-pid*
	   #:*fn-log-info* #:*fn-log-err* #:*fn-log-trace* #:*log-prefix*
	   #:add-daemon-log #:get-daemon-log-list
	   #:*print-log-datetime* #:*fn-log-pid*
	   #:*disabled-functions-logging*
	   #:*disabled-layers-logging*
	   #:*process-type*))

(in-package :daemon-logging)

;;; Logging configure parameters
(declaim (type fixnum *log-indent* *log-indent-size*))
(defparameter *log-indent* 0)
(defparameter *log-indent-size* 2)

(defparameter *print-log-info* t)
(defparameter *print-log-err* t)
(defparameter *print-log-layer* t)
(defparameter *print-internal-call* t)
(defparameter *print-call* t)
(defparameter *print-called-form-with-result* t)
(defparameter *disabled-functions-logging* nil)
(defparameter *disabled-layers-logging* nil)
(defparameter *log-prefix* nil)
(defparameter *print-log-datetime* nil)
(defparameter *print-trace-function* nil)
(defparameter *print-pid* t)
(defparameter *simple-log* nil)

(defparameter *process-type* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Logging for actions on differently layers, for using
;;; defining +log-layer+ (if it not defining then reading name of current package), 
;;; *fn-log-info*, and *fn-log-err* special variables. Example:

(defconstant +log-layer+ :logging-layer)
(defparameter *fn-log-info* #'(lambda (fmt-str &rest args)
				(apply #'format t fmt-str args)))
(defparameter *fn-log-err* #'(lambda (fmt-str &rest args)
				(apply #'format t fmt-str args)))
(defparameter *fn-log-trace* #'(lambda (fmt-str)
				(funcall #'princ fmt-str)))
(defparameter *fn-log-pid* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;; Logging native parameter ;;;;;;;;;;
;;; For added log strings
(defparameter *daemon-logs* nil)

;;; For defun-ext and wrap-fmt-str ;;;;;;;;;;;;;;;;;;
(defparameter *def-in-package* nil)
(defparameter *trace-fn* nil)

;;; For pointing logging mode ;;;;;;;;;;;;;;;;;;;;;;
(defparameter *log-mode* :info "Must be (or :trace :info :error)")
(defparameter *trace-type* nil "Must be (or :call :result)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-daemon-log (log)
  (push log *daemon-logs*)
  log)

(defun get-daemon-log-list ()
  (nth-value 0 (read-from-string (format nil 
				       "(~A)"
				       (apply #'concatenate 'string (reverse *daemon-logs*))))))

(defun get-indent ()
  (make-string *log-indent* :initial-element #\Space))

(defun get-log-fn (fn-log-str)
  (let ((sym (when *def-in-package* (find-symbol fn-log-str *def-in-package*))))
    (if (and sym (boundp sym))
	(symbol-value sym)
	(symbol-value (find-symbol fn-log-str (load-time-value *package*))))))

(defun get-fn-log-info ()
  (get-log-fn (symbol-name '*FN-LOG-INFO*)))
(defun get-fn-log-err ()
  (get-log-fn (symbol-name '*FN-LOG-ERR*)))
(defun get-fn-log-trace ()
  (get-log-fn (symbol-name '*FN-LOG-TRACE*)))

(defun get-log-layer ()
  (let ((layer-sym (when *def-in-package* (find-symbol (symbol-name '+LOG-LAYER+) *def-in-package*))))
    (if (and layer-sym (boundp layer-sym)) 
	(symbol-value layer-sym)
	(read-from-string (concatenate 'string ":" (package-name (or *def-in-package* *package*)))))))

;;; Checking
;(get-log-layer)
;(log-info "test")
;(log-info "test: ~S")
;(syslog-info "test")
;(defun-ext f (x y) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4)
;;;;;;;;;;;;;;;;;

(defun get-datetime ()
  (multiple-value-bind (sec min hour date month year)
      (get-decoded-time)
    (format nil "~D.~2,'0D.~2,'0D ~2,'0D:~2,'0D:~2,'0D" year month date hour min sec)))

(defmacro create-log-plist (&rest details)
  (cons 'append 
	(loop 
	   for detail in details
	   for key = (first detail)
	   for message = (second detail)
	   if (= 2 (length detail)) do (setq detail (append detail '(t)))
	   collect `(when ,(third detail) (list ,key ,message)))))

(defun wrap-fmt-str (fmt-str &key extra-fmt-str (indent "") &aux log-plist)
  (when *simple-log* 
    (return-from wrap-fmt-str fmt-str))
  (setq log-plist
	(create-log-plist
	 (:daemonization *log-mode*)
	 (:message fmt-str (member *log-mode* '(:info :error)))
	 (:call fmt-str (and (eq *log-mode* :trace) (eq *trace-type* :call) *print-call*))
	 (:result fmt-str (and (eq *log-mode* :trace) (eq *trace-type* :result) *print-call*))
	 (:called-form extra-fmt-str (and (eq *log-mode* :trace) (eq *trace-type* :result) *print-call* *print-called-form-with-result*))
	 (:datetime (get-datetime) *print-log-datetime*)
	 (:pid (funcall *fn-log-pid*) *print-pid*)
	 (:layer (get-log-layer) *print-log-layer*)
	 (:trace-fn *trace-fn*)
	 (:type-proc *process-type*)))
  (format nil
	  "~%(~{~A~})"
	  (loop 
	     with begin-str = (format nil 
				      "~S ~A"
				      (first log-plist) 
				      (if (eq :info (second log-plist))
					  (progn (setq log-plist (rest (rest log-plist)))
						 (format nil "~6A ~A ~S" "" indent (second log-plist)))
					  (format nil "~6S ~A" (second log-plist) indent)))
	     for pair on (rest (rest log-plist)) by #'cddr 
	     collect (if (member (first pair) '(:call :result :called-form))
			 (format nil " ~S ~A" (first pair) (second pair))
			 (format nil " ~S ~S" (first pair) (second pair))) 
	     into result 
	     finally (return (cons begin-str result)))))

(defun logging (fn-log format-str &rest args)
  (let ((*print-pretty* nil)) 
    (apply fn-log (funcall #'wrap-fmt-str format-str :indent (get-indent)) args)))

(defmacro log-info (format-str &rest args)
  `(let ((fn-log (get-fn-log-info)))
     (when (and fn-log *print-log-info*)
       (let ((*def-in-package* (load-time-value *package*))
	     (*log-mode* :info))
	 (logging fn-log ,format-str ,@args)))))

(defmacro log-err (format-str &rest args)
  `(let ((fn-log (get-fn-log-err)))
     (when (and fn-log *print-log-err*)
       (let ((*def-in-package* (load-time-value *package*))
	     (*log-mode* :error))   
	 (logging fn-log ,format-str ,@args)))))

(defun syslog-trace (form-str &key extra-form-str trace-type)
  (declare (type (member :call :result) trace-type)
	   (type string form-str)
	   (type (or null string) extra-form-str))
  (let ((*log-mode* :trace)
	(*trace-type* trace-type)
	(*print-pretty* nil))
    (funcall (get-fn-log-trace) (funcall #'wrap-fmt-str
					 form-str
					 :extra-fmt-str extra-form-str
					 :indent (get-indent)))))
			        
(defun syslog-call-into (form-str)
  (syslog-trace form-str :trace-type :call)
  (incf *log-indent* *log-indent-size*))
  
(defun syslog-call-out (result-form-str &optional called-form-str)  
  (decf *log-indent* *log-indent-size*)
  (if (< *log-indent* 0)
      (error "*log-indent* not must be less zero. Not correct log operation."))
  (syslog-trace result-form-str :extra-form-str called-form-str :trace-type :result))
  
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

  (defun is-special-or-macro-p (fn-sym)
    (or (special-operator-p fn-sym) (macro-function fn-sym)))
  );eval-when 

(defun correct-sym (sym)
    (format nil 
	    (if (eql (symbol-package sym)
		     (find-package *def-in-package*))
		"~A"
		"~S")
	    sym))

#|(defun present-function (function)  
  (third (multiple-value-list (function-lambda-expression function))))
|#

(defun object-is-not-printable-p (obj)
  (handler-case (let ((*print-readably* t)) 
		  (format nil "~S" obj)
		  nil)
    (print-not-readable () t)))

(defun str-list-close (str)
  (concatenate 'string
	       (subseq str 0 (1- (length str)))
	       ")"))

(defun present-form (&optional form &rest extra-forms)
  (cond 
    ((null form) "NIL")
    ((null extra-forms)
     (cond 
       ((consp form)  (if (and (= 2 (length form)) 
			       (eq 'quote (first form)))
			  (format nil "'~A" (present-form (second form)))
			  (str-list-close 
			   (format nil "(~{~A ~}" (mapcar #'present-form form)))))
       ((symbolp form) (correct-sym form))
       ;((functionp form) (format nil "~S" (present-function form)))    
       (t (format nil 
		  (if (object-is-not-printable-p form)
		      "|~S|"
		      "~S")
		  form))))
     (t (str-list-close (format nil "(:VALUES ~{~A ~}" (mapcar #'present-form (cons form extra-forms)))))))

(defun is-logging-p (fn-sym package)
  (and (not (member fn-sym *disabled-functions-logging*))
       (not (member (package-name package)
	       *disabled-layers-logging* 
	       :test #'string-equal
	       :key #'princ-to-string))))

(defmacro wrap-log-form (form)
  (with-gensyms (form-str res fn args)
    `(if (not *print-internal-call*)
	 ,form
	 (let* ((*def-in-package* (load-time-value *package*))
		(*log-indent* *log-indent*)
		(,fn ',(first form))
		,@(unless (is-special-or-macro-p (first form))
			 `((,args (list ,@(rest form)))))
		(,form-str (present-form 
			    ,(if (is-special-or-macro-p (first form)) 
				 `(quote ,form)
				 `(cons ,fn ,args)))))
	   (when (is-logging-p ,fn *def-in-package*)
	     (syslog-call-into ,form-str))
	   (let ((,res (multiple-value-list 
			,(if (is-special-or-macro-p (first form))
			     form
			     `(apply ,fn ,args)))))
	     (when (is-logging-p ,fn *def-in-package*)
	       (syslog-call-out (apply #'present-form ,res) (when *print-called-form-with-result* ,form-str)))
	     (apply #'values ,res))))))

(defmacro wrap-log (&rest forms)
  `(progn ,@(loop for form in forms
	       collect `(wrap-log-form ,form))))

(defmacro defun-ext (name args &body body)
  (with-gensyms (form-str res this-name)
    `(defun ,name ,args
       (let* ((,this-name ',name)
	      (*def-in-package* (load-time-value *package*))
	      (*trace-fn* ',name)
	      (*log-indent* *log-indent*)
	      (,form-str (present-form (cons ',name 
					     (mapcar #'(lambda (arg)
							 (if (consp arg)
							     (list 'quote arg)
							     arg))
						     (list ,@(present-args args)))))))
	 (when (and *print-call* (is-logging-p ,this-name *def-in-package*))
	   (syslog-call-into ,form-str))

	 (let ((,res (multiple-value-list 
		      (locally ,@(remove-declare-ignore body)))))	   
	   (when (and *print-call* (is-logging-p ,this-name *def-in-package*))
	     (apply #'syslog-call-out (apply #'present-form ,res) (when *print-called-form-with-result* (list ,form-str))))
	   (apply #'values ,res))))))

;(defun-ext f (x y &key z) (log-info "this f") (+ x z (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4 :z 1)
