(defpackage testpkg1 (:use :cl) (:export #:f))

(in-package :testpkg1)

(defun f (file &aux fd)
  (sb-posix:umask 0)
  (setq fd (sb-posix:open file (boole boole-ior sb-posix:O-RDWR sb-posix:O-CREAT) #b110110110))
  (sb-posix:close fd)
  (trivial-shell:shell-command (format nil "ls -l ~A" file)))

(defpackage testpkg2 (:use :cl :testpkg1))

(in-package :testpkg2)

(defun g (&aux file)
  (setq file  "/home/lispuser/tmp/test-file")
  (when (probe-file file) (delete-file file))
  (f file))

(g)