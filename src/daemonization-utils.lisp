(defpackage :daemonization-utils
  (:import-from :daemon-utils-port #:get-args #:getpid #:recreate-file-allow-write-other)
  (:export #:get-args #:get-proc-id #:recreate-file-allow-write-other))

(setf (symbol-function 'get-proc-id) #'getpid)
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