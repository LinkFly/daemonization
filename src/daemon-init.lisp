(defpackage :daemon-init 
  (:use :cl :daemon-core-port))

(in-package :daemon-init)
 
;;; Initialization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-sys-functions-and-constants)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;