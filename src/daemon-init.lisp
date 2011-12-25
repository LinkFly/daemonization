(defpackage :daemon-init 
  (:use :cl :daemon-core-port))

(in-package :daemon-init)
 
;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-unix-functions)
(import-sys-functions-and-constants)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;