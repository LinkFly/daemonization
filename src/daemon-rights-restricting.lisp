(defpackage :daemon-right-restricting
  (:use :cl :daemon-utils-port)
  (:export #:restrict-rights
   ))

(in-package :daemon-right-restricting)

(defun restrict-rights (&key new-user new-group)
    (preparation-before-grant)
    (when new-user
      (change-user new-user new-group))
    (set-grant))
