(defpackage :daemonization-utils
  (:import-from :daemon-utils-port #:get-args #:getpid #:recreate-file-allow-write-other)
  (:export #:get-args #:getpid #:recreate-file-allow-write-other))
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