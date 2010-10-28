(defpackage :daemon-required-features
  (:use :cl)
  (:export #:req-feature-p))

(in-package :daemon-required-features)

(defparameter *required-features*
  '(:change-user t 
    :listen-privileged-port t
    :switch-to-pts t
    :as-daemon t))

(defun req-feature-p (feature)
  (getf *required-features* feature))


  




   