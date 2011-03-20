(push #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/" asdf:*central-registry*)
(push #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/restart-process/" asdf:*central-registry*)

(asdf:load-system :restart-process)
(asdf:load-system :sb-posix)
(daemonization:daemonized "start" 
;    :user "lispuser" 
    :pid-file "/home/lispuser/tmp/pid" 
    :main-function #'(lambda () 
		       (load "/media/WORK_PARTITION/work_dir/web-projects/development/lisp/start-swank.lisp"))
    :before-parent-exit-fn #'(lambda (pid status)			       
			       (unless (= status sb-posix:sigchld)
				 (restart-process:restart-process-on-stops pid
									   :revision-timeout 3))))


