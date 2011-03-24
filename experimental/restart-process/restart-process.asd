(defsystem :restart-process 
  :depends-on (:split-sequence :sb-posix) ;:trivial-shell)
  :components ((:file "restart-process")))