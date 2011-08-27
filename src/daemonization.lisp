;(print (find-package :sb-introspect))
;(require :sb-introspect)
;(require :sb-debug)
(defpackage :daemonization 
  (:use :cl :daemon-share :daemon-core-port) ;:sb-introspect :sb-debug)
  (:import-from :daemon-utils-port #:exit #:getpid)
  (:export #:daemonized #:get-daemon-log-list #:*fn-log-info* #:*fn-log-err* #:*fn-log-trace*
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
	(*fn-log-err* nil)
	(*print-call* nil))
    (correct-and-check-conf-params conf-params #'check-conf-params)))

(defun-ext check-and-correct-pid-file-param (conf-params daemon-command)
  (let ((pid-file (getf conf-params :pid-file))
	(pid-file-dir (getf conf-params :pid-file-dir)))	  
    (when pid-file
      (unless (absolute-path-p pid-file)
	(unless pid-file-dir (setf pid-file-dir (get-pid-files-dir)))
	(setf (getf conf-params :pid-file) 
	      (setf pid-file (make-pathname :defaults (pathname-as-directory pid-file-dir)
					    :name pid-file)))))))

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


(defun f () 
  ;(error "my-test-error")
  )

(declaim (ftype (function ((or pathname string null config-plist) string &key 
			   (:on-error (member :return-error :as-ignore-errors :call-error :exit-from-lisp)) 
			   (:recreate-pid-file-on-start t) (:print-extra-status t)))
		daemonized))
(defun-ext daemonized (conf-params daemon-command &key (on-error :call-error) recreate-pid-file-on-start print-extra-status 
		       &aux (on-error-variants '(:return-error :as-ignore-errors :call-error :exit-from-lisp)))
  (let ((conf-params conf-params))
    (let ((pathname (get-system-path)))
      (unless (check-normal-start-pathname pathname) (call-bad-start-pathname-error pathname)))      
    (handler-case 
	(progn 
	  ;(error "my-test-error")
	  (f)

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
	      (analized-and-print-result result daemon-command conf-params :print-extra-status print-extra-status)
	      (let (status extra-status)
		(if (consp result)
		    (setq status (first result)
			  extra-status (second result))
		    (setq status result))
		(when (getf conf-params :exit)
		  (exit status))
		(values status extra-status)))))
      (error (err)
	(when (eq :child *process-type*) (error err))
	(let ((err-str (format nil "~A ~S~%Failed (ERROR: ~A)" err (get-error-description err) err)))
	  (when (find #\~ err-str) 
	    (error "Bad error message - it is contain the tildes. Error message: \"~A\"." err-str))
	  (log-err err-str)
	  (format t "ERROR: ~A~%" err-str))
					
	;;;;;;;;;;;;;;;;;
	(defun f0 (x) (1+ x) (error "test-error"))
	(defun f1 (x) (f0 x))
	(defun f2 ()
	  (block error-trap
	    (handler-bind ((error (lambda (err) (return-from error-trap (get-error-description err)))))
	      (f1 0))))
	;(format t "~%ERROR DESCRIPTION:~%~S~%" (f2))
       ;;;;;;;;;;;;;;;;;

	(case on-error
	  (:exit-from-lisp (exit +ex-general+))
	  (:call-error (error err))
	  (:return-error err)
	  (:as-ignore-errors (values nil err)))))))

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
(change-user ...) 
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



