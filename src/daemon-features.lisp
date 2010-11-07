(defpackage :daemon-features
  (:use :cl))

(in-package :daemon-features)

(eval-when (:compile-toplevel)
  (defparameter *enabled-features*
    '(:daemon.change-user t
      :daemon.listen-privileged-ports t
      :daemon.as-daemon t))

  (loop for (feature enabled-p . nil) on *enabled-features* by #'cddr
     when enabled-p do (pushnew feature *features*)))

