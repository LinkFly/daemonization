(defpackage :daemonization-utils
  (:use :cl)
  (:import-from :daemon-share #:print-call-p #:with-tmp-logger)
  (:import-from :daemon-utils-port #:get-args #:getpid #:recreate-file-allow-write-other)
  (:export #:get-args #:get-proc-id #:recreate-file-allow-write-other))

(in-package :daemonization-utils)

(defun get-proc-id ()
  (with-tmp-logger ((print-call-p nil)) (getpid)))

#| 
daemoniation.lisp:
check-daemon-command
check-conf-params

;daemon-share
ensure-absolute-path

case-command

;daemon-core-port
{x}-service

analized-and-print-result
|#