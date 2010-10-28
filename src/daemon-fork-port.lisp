(defvar *status* nil)

(defun signal-handler (sig info context)
  (declare (ignore info context))
  (setf *status* sig))

  (sb-sys:enable-interrupt sb-posix:sigusr1 #'signal-handler)
  (sb-sys:enable-interrupt sb-posix:sigchld #'signal-handler)

(unless (= (sb-posix:fork) 0)
    (loop
       while (null *status*)
       do (sleep 0.1))
    (quit :unix-status (if (= *status* sb-posix:sigusr1)
                           0
                           1)))

(sb-sys:enable-interrupt sb-posix:sigusr1 :default)
(sb-sys:enable-interrupt sb-posix:sigchld :default)

(sb-posix:kill *ppid* sb-posix:sigusr1)

