(defpackage :daemon-init 
  (:use :cl :daemon-core-port))

(in-package :daemon-init)

;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(logging-init)
(import-sys-functions-and-constants)
(define-unix-functions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;