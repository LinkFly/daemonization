;(print (find-package :sb-introspect))
;(require :sb-introspect)
;(require :sb-debug)
(defpackage :daemonization 
  (:use :cl :daemon-share :daemon-core-port) ;:sb-introspect :sb-debug)
  (:import-from :daemon-utils-port #:exit #:getpid)
  (:export #:daemonized #:get-daemon-log-list #:*fn-log-info* #:*fn-log-err* #:*fn-log-trace* #:*fn-log-info-load*
	   #:+all-daemon-commands+ #:+conf-parameters+
	   ;; for reading extra-status
	   #:extra-status
	   #:make-extra-status
	   #:extra-status-p
	   #:copy-extra-status
	   #:extra-status-pid
	   #:extra-status-exit-code
	   #:extra-status-name
	   #:extra-status-pid-file
	   #:extra-status-user

	   ;; for finding pid-files
	   #:*pid-files-dirname* #:get-pid-files-dir

	   ;; for finding conf-files 
	   #:*conf-files-dirname* #:get-conf-files-dir #:*default-conf-file-name* #:get-default-conf-file

	   ;;for analizing conf-params
	   #:probe-conf-params
	   #:equal-conf-params))

(in-package :daemonization)

;;;;; Initialization ;;;;;;;;;;
;;; For correct reset :line property (in log-plist, in call function in slot's fn-correct-log-plist of *logger*)
(setf *main-function-symbol* 'daemonized)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging checking
;(log-info "test")
;(defun-ext f (x y) (log-info "this f") (+ x (g y)))
;(defun-ext g (x) (log-info "this g") (* x x))
;(f 3 4)
;(daemon-logging::f 3 4 :z 1)
;(defun f (x &key y) (+ x y))
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant +all-daemon-commands+ '("start" "stop" "zap" "kill" "restart" "nodaemon" "status")) 

(define-constant 
    +bad-conf-str-err+
    (format nil 
	    "Bad argument conf-params. Configure parameters must be path to file with property list (plist) or self be plist. Plist must be with any keys from ~S. Example: '(:name \"mydaemon\" :user \"username\" :pid-file \"/home/username/tmp/pid\")"
	    +conf-parameters+))
(define-constant 
    +bad-cmd-str-err+ 
    (format nil "Bad daemon command. Command must be one of the ~S"
	    +all-daemon-commands+))

(defun-ext check-daemon-command (cmd)
  (log-info "Check daemon command ...")
  (unless (find cmd +all-daemon-commands+ :test #'string-equal)
    (log-err +bad-cmd-str-err+)
    (error +bad-cmd-str-err+))
  (log-info " ... OK.")
  cmd)

(defun-ext check-conf-params (conf-params &optional cmd)
;(setq conf-params '(:name "mydaemon" :user "lispuser" :pid-file "/home/lispuser/tmp/pid"))
  (log-info "Check conf-params ...")
  (unless (typep conf-params 'config-plist)
    (let ((err-str (format nil
			   "~A~A"
			   +bad-conf-str-err+
			   (if (null cmd)
			       ""
			       (format nil " (daemon command: ~A)" cmd)))))
      (log-err err-str)
      (error err-str)))
  (log-info " ... OK.")
  conf-params)		       

(defun-ext read-conf-params (file)
  (with-open-file (stream file)
    (read stream)))

(defun-ext merge-conf-params (base-params params)
  (loop 
     with result-params = (copy-tree base-params)
     for pair on params by #'cddr
     do (setf (getf result-params (first pair)) 
	      (second pair))
     finally (return result-params)))

(defun-ext sort-conf-params (conf-params) ;
  (setf conf-params (copy-tree conf-params))
  (loop 
     with pairs = (sort (loop for key-val-rest on conf-params by #'cddr
			   for (key val) = key-val-rest
			   collect (list key val))
			#'string<
			:key (lambda (pair) (symbol-name (first pair))))
     for pair in pairs
     append pair)) ;sort-conf-params

(defun-ext equal-conf-params (conf-params1 conf-params2)  
  (equalp (sort-conf-params conf-params1) (sort-conf-params conf-params2)))

(defun-ext normalize-conf-params (params-or-file)
  (declare (type (or pathname string list) params-or-file))
  (typecase params-or-file
    (list (return-from normalize-conf-params params-or-file))
    ((or pathname string) 
     (with-open-file (stream (get-real-file params-or-file (get-conf-files-dir)))
       (read stream)))))

(defun-ext correct-and-check-conf-params (conf-params fn-check)
  (loop 
     with result
     with conf-files-dir = (get-conf-files-dir)
     for cur-conf-params = (funcall fn-check (normalize-conf-params conf-params))
       then (funcall fn-check (read-conf-params file))
     for file = (get-real-file (getf cur-conf-params :parent-conf-file)
			       (or (getf cur-conf-params :parent-conf-file-dir)
				   conf-files-dir))
     while file
     do (log-info "~%Found parent file: ~A" file)
     if (member file parent-files :test #'equal)
      do (error "Cyclic dependency in the config files")
     else collect file into parent-files 
     do (push cur-conf-params result)
     finally (return
	       (sort-conf-params
		(progn 
		  (push cur-conf-params result)
		  (do ((cur-params (funcall fn-check 
					    (read-conf-params (get-default-conf-file)))
				   (merge-conf-params cur-params (pop result))))
		      ((null result) cur-params)))))))

(defun-ext probe-conf-params (conf-params)   
  (let ((*fn-log-info* nil)
	(*fn-log-err* nil))
    (with-tmp-logger ((print-call-p nil))
      (correct-and-check-conf-params conf-params #'check-conf-params))))

(defun-ext check-and-correct-pid-file-param (conf-params daemon-command)
  (let ((pid-file (getf conf-params :pid-file))
	(pid-file-dir (getf conf-params :pid-file-dir)))	  
    (when pid-file
      (setf (getf conf-params :pid-file) 
	    (get-real-file pid-file 
			   (if pid-file-dir pid-file-dir #'get-pid-files-dir))))))

(defun-ext check-not-exists-pid-file (pid-file cmd)
  (let ((err-str "pid-file ~S is already exists (daemon command: ~A)"))
    (when (probe-file pid-file)
      (log-err err-str pid-file cmd)
      (error err-str pid-file cmd)
      )))

(defmacro result-messages (cmd-sym result-sym alist-messages extra-prompts print-extra-prompts)
  ;(setq alist-messages `(((,+ex-ok+ "status" "kill") "fmt-str ~S ~A" 3434 data) (("stop" ,+ex-software+) "fmt-str ~S" 3434)))
  ;(setq cmd-sym 'cmd)
  ;(setq result-sym 'result)
  `(cond 
     ,@(loop for ((result . cmds) fmt-str . args) in alist-messages
	  collect `((and (= ,result-sym ,result)
			 (or ,@(loop for cmd in cmds
				  collect `(string= ,cmd-sym ,cmd))))
		    (format nil "~A~A" 
			    (funcall #'format nil ,fmt-str ,@args)
			    (if (and ,print-extra-prompts ,extra-prompts) (format nil " ~S" ,extra-prompts) ""))))))
(defmacro for-create-result-plist (cmd-sym result-sym alist-messages internal-result)
  `(cond 
     ,@(loop for ((result . cmds) result-val . key-val-pairs) in alist-messages
	  collect `((and (= ,result-sym ,result)
			 (or ,@(loop for cmd in cmds
				  collect `(string= ,cmd-sym ,cmd))))
		    (append (list :result ,result-val
				  :command ,cmd-sym)
			    (list ,@key-val-pairs)
			    (list :internal-result ,internal-result))))))

(declaim (ftype (function (status extra-status string config-plist)) create-result-plist))
(defun-ext create-result-plist (status extra-status cmd conf-params)
  (log-info "creating result plist  ...")
  (let* ((result-plist
	  (for-create-result-plist
	   cmd 
	   status
	   (((+ex-software+ "start") "failed"
	     :status nil :reason (if (and (extra-status-exit-code extra-status) 
					  (= (extra-status-exit-code extra-status) 
					     +ex-cantcreate+))
				     "pid file already exists"
				     "internal-error")
	     :pid nil :pid-file (getf conf-params :pid-file))
	    ((+ex-software+ "stop" "zap" "kill" "restart" "status") "failed"
	     :status nil :reason "internal-error"
	     :pid nil :pid-file (getf conf-params :pid-file))
	    ((+pid-file-not-found+ "start" "stop" "zap" "kill" "restart") "failed"
	     :status nil :reason "no-pid-file" 
	     :pid nil :pid-file (getf conf-params :pid-file))
	    ((+process-not-exists+ "stop") "failed"
	     :status nil :reason "not-running" 
	     :pid (extra-status-pid extra-status) :pid-file (extra-status-pid-file extra-status))	  
	   
	    ((+pid-file-not-found+ "status") "success"
	     :status "not-running" :reason "no-pid-file" 
	     :pid nil :pid-file (getf conf-params :pid-file))
	    ((+ex-ok+ "status") "success"
	     :status "running" :reason nil 
	     :pid (extra-status-pid extra-status) :pid-file (getf conf-params :pid-file))
	    ((+ex-unavailable+ "status") "success"
	     :status "not-running" :reason "service-unavailable" 
	     :pid (extra-status-pid extra-status) :pid-file (getf conf-params :pid-file))

	    ((+ex-ok+ "start") "success"
	     :status nil :reason nil
	     :pid (extra-status-pid extra-status) :pid-file (extra-status-pid-file extra-status))
	    ((+ex-ok+ "stop" "zap" "kill" "restart") "success"
	     :status nil :reason nil
	     :pid (extra-status-pid extra-status) :pid-file (extra-status-pid-file extra-status)))
	   (list :status status :extra-status extra-status))))
    (log-info "Ok. result-plist: ~S" result-plist)
    result-plist))

(defun-ext declension-cmd (cmd)
  (cond 
    ((string-equal "stop" cmd) "stopped")
    ((string-equal "zap" cmd) "zapped")
    ((string-equal "kill" cmd) "killed")
    ((string-equal "start" cmd) "started")
    ((string-equal "restart" cmd) "restarted")))       
  
(defun-ext result-plist-simple-printer (result-plist &optional print-internal-result)
  (with-keys (result command status reason pid pid-file internal-result) 
      (copy-list ;'(:result "success" :command "start" :status "started" :reason nil :pid nil :pid-file nil)
       result-plist)
    (macrolet ((to-strings (&rest getters)
		 `(progn ,@(loop for getter in getters
			      collect `(setf ,getter (princ-to-string ,getter)))))
	       (if-not-nil (val &body body)
		 `(if (string/= "NIL" ,val) 
		      (progn ,@body)
		      ""))
	       (print-pid-and-pid-file () 
		 `(if (not (or (string/= pid "NIL")
			       (string/= pid-file "NIL")))
		      ""
		      (concatenate 'string " ("
				   (if-not-nil pid 
					       (concatenate 'string
							    "pid = "
							    pid))
				   (if-not-nil pid-file 
					       (concatenate 'string
							    (if-not-nil pid " ")
							    "pid-file = "
							    pid-file))
				   ")"))))
      (to-strings result command status reason pid pid-file)
      (format nil 
	      "~&~A~A~%" 
	      (cond 
		((string-equal "success" result)
		 (cond 
		   ((string-equal "status" command)
		    (concatenate 'string status
				 (if-not-nil reason (concatenate 'string " - " reason))
				 (print-pid-and-pid-file)))
		   ((member command '("start" "restart" "stop" "kill" "zap") :test #'string-equal)
		    (concatenate 'string result " " (declension-cmd command) (print-pid-and-pid-file)))))
		((string-equal "failed" result)
		 (concatenate 'string result " " (declension-cmd command)
			      (if-not-nil reason (concatenate 'string " - " reason))
			      (print-pid-and-pid-file))))
	      (if (not print-internal-result)
		  ""
		   (format nil " internal-result - ~S" internal-result))))))

(defun-ext print-result-plist (result-plist &key print-type print-internal-result)
  (declare (type result-plist result-plist)
	   (type (member :simple :none :plist) print-type))
  (case print-type
    (:none)
    (:plist (format *standard-output* "~S" result-plist))
    (:simple (princ (result-plist-simple-printer result-plist print-internal-result)
		    *standard-output*))))
     
(defun-ext analized-and-print-result (result cmd conf-params &key print-extra-status &aux status extra-status)
  (declare (ignorable conf-params))
  (log-info "here analizing result ...")
  (if (consp result)
      (setq status (first result)
	    extra-status (second result))
      (setq status result))
  (let ((analize-str
	 (result-messages cmd 
			  status
			  (((+ex-software+ "start") "failed ~A~A" cmd (if (and (extra-status-exit-code extra-status) 
									     (= (extra-status-exit-code extra-status) 
										+ex-cantcreate+))
									(format nil " - pid file already exists ~A"
										(getf conf-params :pid-file))
									""))
			   ((+pid-file-not-found+ "status") "not-running - no pid file")
			   ((+pid-file-not-found+ "start" "stop" "zap" "kill" "restart") "failed ~A - no pid file" cmd)
			   ((+ex-ok+ "status") "running (pid = ~A)" (extra-status-pid extra-status))
			   ((+ex-unavailable+ "status") "not-running") 
			   ((+process-not-exists+ "stop") "failed stop - not-running (pid = ~A pid-file = ~A)" 
			                          (extra-status-pid extra-status) (extra-status-pid-file extra-status))
			   ((+ex-ok+ "start") "success started (pid = ~A)" (extra-status-pid extra-status))
			   ((+ex-ok+ "stop" "zap" "kill" "restart") "success ~A" cmd) 
			   ((+ex-software+ "stop" "zap" "kill" "restart" "status") "failed ~A" cmd))
			  extra-status
			  print-extra-status)))
    (log-info analize-str)
    (format t "~A~%" analize-str)
    (log-info " ... ok"))
  result)

(defmacro case-command (cmd &rest cases-bodies)
  `(cond 
     ,@(loop for (cmd-clause . forms) in cases-bodies
	  collect `((string-equal ,cmd ,cmd-clause) ,@forms))))		    

(defun check-normal-start-pathname (pathname)
  (loop for dir in (rest (pathname-directory pathname))
     if (find #\~ dir) do (return nil)
     finally (return t)))

;;; Setted *main-function-symbol* in 'daemonization:daemonized for correct reset :line property (in log-plist, in function 
;;;   slot's fn-correct-log-plist of *logger*). Look at the begin in Initialization.

(declaim (ftype (function ((or pathname string null config-plist) string &key 
			   (:on-error (member :return-error :as-ignore-errors :call-error :exit-from-lisp)) 
			   (:recreate-pid-file-on-start t) 
			   (:print-result-type (member :simple :none :plist))
			   (:print-internal-result t)))
		daemonized))
(defun-ext daemonized (conf-params daemon-command  
		       &key (on-error :call-error) recreate-pid-file-on-start (print-result-type :simple) print-internal-result
		       &aux (on-error-variants '(:return-error :as-ignore-errors :call-error :exit-from-lisp)))
  (let ((conf-params conf-params))
    (let ((pathname (get-system-path)))
      (unless (check-normal-start-pathname pathname) (call-bad-start-pathname-error pathname)))      
    (block error-trap 
      (handler-bind ((error (lambda (err)
			      (when (eq :child *process-type*) (error err))
			      (let ((err-str (format nil "~A ~S~%failed (ERROR = ~A)" err (get-error-description err) err)))
				(when (find #\~ err-str) 
				  (error "Bad error message - it is contain the tildes. Error message: \"~A\"." err-str))
				(log-err err-str)
				(format t "ERROR: ~A~%" err-str))
			      (case on-error
				(:exit-from-lisp (exit +ex-general+))
				(:call-error (error err))
				(:return-error err)
				(:as-ignore-errors (values nil err)))
			      (return-from error-trap))))
	(progn 
	  (when (consp conf-params) (setf conf-params (copy-list conf-params)))  

	  ;; Checking normal parameters, command, and start/load pathname 	
	  (assert (member on-error on-error-variants)
		  () 
		  "Bad keyword parameter on-error = ~S. Must be one of the ~S" on-error on-error-variants)
	  (check-daemon-command daemon-command)

;;; Set the conf-params given parent files, check and correct conf-params
	  (setq conf-params (correct-and-check-conf-params conf-params 
							   (lambda (conf-params) 
							     (check-conf-params conf-params
										daemon-command))))
	
	  (check-and-correct-pid-file-param conf-params daemon-command)

	  (log-info "conf-params is: ~S" conf-params)

;;; Recreate or check is exists pid-file
	  (let ((pid-file (getf conf-params :pid-file)))
	    (when (string-equal "start" daemon-command)
	      (if recreate-pid-file-on-start
		  (when (probe-file pid-file) (delete-file pid-file))
		  (check-not-exists-pid-file pid-file daemon-command))))
	
	  (let* ((*process-type* :parent)
		 (result (block into-daemonized
			   (let ((*fn-exit* #'(lambda (&optional (status +ex-ok+) extra-status) 
						(return-from into-daemonized (list status extra-status)))))
			     (the config-plist conf-params)
			     (case-command daemon-command
					   ("zap" (zap-service conf-params))
					   ("stop" (stop-service conf-params))
					   ("kill" (kill-service conf-params))
					   ("restart" (let ((res (block into-daemonized-lev2
								   (let ((*fn-exit* #'(lambda (&optional (status +ex-ok+) extra-status)
											(return-from into-daemonized-lev2
											  (list status extra-status)))))
								     (stop-service conf-params)))))
							(if (if (numberp res) 
								(= +ex-ok+ res)
								(= +ex-ok+ (first res)))
							    (let ((res (start-service conf-params)))
							      (funcall *fn-exit* res)))))
					   ("start" (start-service conf-params))		   
					   ("nodaemon" (simple-start conf-params))
					   ("status" (status-service conf-params)))))))
	    (when (eq :parent *process-type*)	      
	      (let (result-plist status extra-status)
		(if (consp result)
		    (setq status (first result)
			  extra-status (second result))
		    (setq status result))
		(setf result-plist (create-result-plist status extra-status daemon-command conf-params)) ;:print-extra-status print-extra-status))
		(print-result-plist result-plist :print-type print-result-type :print-internal-result print-internal-result)
		;(analized-and-print-result result daemon-command conf-params :print-extra-status print-extra-status)
		(when (getf conf-params :exit)
		  (exit status))
		result-plist))))))))

#|
- Действия необходимые для отсоединения от терминала
     - определение константы sb-unix:tiocnotty
     ...
- Действия необходимые чтобы открыть зарезервированный порт (< 1024)
     - определение константы +PR_SET_KEEPCAPS+
     ...
;;;;;;;;;;;
3. Получение конфигурационных параметров.
4. Проверка на корректность комманды.
;;;;;;;;;;;;;
5. Обработка комманд.
    - zap - удалить pidfile и выйти.
    - stop - остановить демон и выйти
       Остановить демон:
       - прочитать pid 
       - послать пользовательский сигнал
       - в цикле посылать сигнал 0, как только появляется отсутствие ошибки - выйти из цикла
       - удалить файл pidfile.
    - restart - остановить демон, и далее стартовать демон.
       Стартовать демон:
       - определить сл.системные ф-ии: grantpt unlockpt ptsname initgroups, prctl, cap_from_text cap_set_proc cap_free
       - установить обработчик(один и тот же) на сигналы: sigusr1, sigchld
       - установить флаг сохранения возможностей процесса, после смены идентификаторов
       - сменить пользователя
       - установить возможность CAP_NET_BIND_SERVICE=ep
       - демонизация
          - fork
             - fork
             - сравнение с нулём, 
                 - если нуль то это потомок, перепрыгиваем и идём дальше
                 - если нет, то
                      - создаём цикл, и не выходим пока статус = nil
                      - как только *status* не равен nil
                            - если равен sigusr1, то выходим с успехом (0)
                            - если не равен sigusr1 то выходим с ошибкой (1)
        Далее, для потомка:
          - получаем ppid
          - меняем *debugger-hook* на глобальный, свой, обработчик ошибок
          - меняем обработчики (обратно) на :default
          - изменяем текущую директорию
          - выставляем маску для создаваемых файлов
          - отсоединяемся от терминала
          - переключаемся к псевдотерминалу
          - создание нового сенса (setsid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Следующее не имеет отношение к настоящей системе, но понадобиться при создании системы restas-daemon-ext базирующейся
на этой системе:
 - настройка ASDF
 - настройка SWANK
 - загрузка restas
 - настройка restas
 - загрузка дополнительных ASDF-систем
 - запуск сайтов
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

          - опять поменять обработчик сигнала sigusr1
             - реакция на остановку демона - логгирование и обработка возможной ошибки.
             - 
          - записать pid в *pidfile*
          - послать родителю сигнал sigusr1
          - установить *debugger-hook к nil
          - логирование о успешном запуске

Что делает no-daemon?
1. Получает аргументы
2. Загружает конфиг
3. Проверяет комманду
4. Устанавливает флаг сохранения возможностей
5. Меняет пользователя
6. Устанавливает CAP_NET_BIND_SERVICE=ep
7. Изменяет текущую директорию
8. Устанавливает маску для создаваемых файлов
9. asdf,swank,restas,sites


Что связано с fork?
1. Установка обработчика signal-handler сигналов sigusr1 и sigchld (для родителя)
2. Fork и ожидание изменение статуса потомком, затем выход (для родителя)
Далее для потомка:
3. Снятие обработчиков сигналов sigusr1, sigchld (установка :default)
- Установка нового обработчика sigusr1 для остановки демона в дальнейшем
4. Сигнал sigusr1 родителя об успешном старте.
|#

;;;;;;; Кроме fork ;;;;;;;
#|
;;Before fork
(prepare-before-grant)
(change-user-and-group ...) 
(set-grant)

... fork ...
set *debugger-hook*
change current dir
set umask
detach-from-tty
switch to pseudo terminal
setsid
enable-interrupt for sigusr1:
 - log-info
 - error -> log-err
 - quit
write pid file
clean *debugger-hook*
|#



