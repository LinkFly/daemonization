(defpackage :daemon-load-foreign-linux-port 
  (:use :cl :daemon-share :daemon-share-port :sb-alien)
  (:export #:import-foreign-functions))

(in-package :daemon-load-foreign-linux-port)
  
(defparameter *stage-str* "")
;; Loading library "libcap" for definition cap-from-text, cap-set-proc and cap-free
#+daemon.listen-privileged-ports
(eval-when (:compile-toplevel :load-toplevel)  
  (eval-when (:compile-toplevel) (setf *stage-str* "Compilation stage. "))
  (eval-when (:load-toplevel) (setf *stage-str* "Loading stage. "))

  (log-info-load "~ASearching libcap in the list ~A" *stage-str* *libcap-probable-libs*)
  (setf *finded-libcap* (find-if #'probe-file *libcap-probable-libs*))
  (log-info-load "~AFinded libcap dynamic library in ~A loading ..." *stage-str* *finded-libcap*)

  (load-shared-object *finded-libcap*)

  (log-info-load "~ALibcap dynamic library ~A loaded." *stage-str* *finded-libcap*))

(defmacro import-foreign-functions ()
  '(let ((*package* (find-package :daemon-load-foreign-linux-port)))
    ;;  Functions for grant capabilities: cap-from-text, cap-set-proc, cap-free
    #+daemon.listen-privileged-ports 
    (progn 
      ;; For compilation following functions, "libcap.so" library must be loaded into 
      ;; compiling system (look at the begining)
      (def-alien-call "cap_from_text" (* char) null-alien (text c-string))
      (def-alien-call "cap_set_proc" int minusp (cap_p (* char)))
      (def-alien-call "cap_free" int minusp (cap_p (* char)))

      (set-cap-functions #'cap-from-text #'cap-set-proc #'cap-free)
      )))

(import-foreign-functions)
 
