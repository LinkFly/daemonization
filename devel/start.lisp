(push #P"/media/WORK_PARTITION/work_dir/web-projects/dynserv/asdf-systems/daemonization/" asdf:*central-registry*)
(asdf:load-system :daemonization)
(daemonization:daemonized "start" 
    :user "lispuser" 
    :pid-file "/home/lispuser/tmp/pid" 
    :main-function #'(lambda () (load "/media/WORK_PARTITION/work_dir/web-projects/development/lisp/start-swank.lisp")))

