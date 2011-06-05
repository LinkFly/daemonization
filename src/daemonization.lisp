(defpackage :daemonization 
  (:use :cl :daemon-share :daemon-core-port)
  (:import-from :daemon-utils-port #:exit #:getpid)
  (:export #:daemonized #:get-daemon-log-list #:*fn-log-info* #:*fn-log-err*))

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
(define-constant +conf-parameters+ '(:main-function :name :user :group :pid-file :before-parent-exit-fn :exit))
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

(defun-ext check-conf-params (conf-params)
;(setq conf-params '(:name "mydaemon" :user "lispuser" :pid-file "/home/lispuser/tmp/pid"))
  (log-info "Check conf-params ...")
  (unless (and (zerop (mod (length conf-params) 2))
	       (loop for key in conf-params by #'cddr
		  if (or (not (keywordp key))
			 (member key keys)
			 (not (member key +conf-parameters+)))
		  do (return)
		  collect key into keys
		  finally (return t)))
      (log-err +bad-conf-str-err+)
      (error +bad-conf-str-err+))
  (log-info " ... OK.")
  conf-params)

(defmacro result-messages (cmd-sym result-sym alist-messages extra-prompts print-extra-prompts)
  ;(setq alist-messages `(((,ex-ok "status" "kill") "fmt-str ~S ~A" 3434 data) (("stop" ,ex-software) "fmt-str ~S" 3434)))
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
	    extra-status (rest result))
      (setq status result))
  (let ((analize-str
	 (result-messages cmd 
			  status
			  (((ex-software "start") "failed ~A~A" cmd (if (and (getf extra-status :exit-code) 
									     (= (getf extra-status :exit-code) 
										ex-cantcreate))
									(format nil " - pid file already exists ~A"
										(getf conf-params :pid-file))
									""))
			   ((+pid-file-not-found+ "status") "not-running - no pid file")
			   ((+pid-file-not-found+ "start" "stop" "zap" "kill" "restart") "failed ~A - no pid file" cmd)
			   ((ex-ok "status") "running (pid = ~A)" (getf extra-status :pid))
			   ((ex-unavailable "status") "not-running") 
			   ((ex-ok "start") "success started (pid = ~A)" (getf extra-status :pid))
			   ((ex-ok "stop" "zap" "kill" "restart") "success ~A" cmd) 
			   ((ex-software "stop" "zap" "kill" "restart" "status") "failed ~A" cmd))
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

(defun-ext daemonized (conf-params daemon-command &key (on-error :call-error) print-extra-status &aux on-error-variants)
  (setq on-error-variants '(:return-error :as-ignore-errors :call-error :exit-from-lisp))
  (assert (member on-error on-error-variants)
	  () 
	  "Bad keyword parameter on-error = ~S. Must be one of the ~S" on-error on-error-variants)
  (handler-case 
      (progn 
	(check-daemon-command daemon-command)
	(when (or (pathnamep conf-params) (stringp conf-params))
	  (setf conf-params
		(with-open-file (stream conf-params)
		  (read stream))))
	(check-conf-params conf-params)  
	(when (getf conf-params :pid-file)
	  (setf (getf conf-params :pid-file) 
		(ensure-absolute-path (getf conf-params :pid-file))))

	(log-info "conf-params is: ~S" conf-params)
	(let* ((*process-type* :parent)
	       (result (block into-daemonized
			 (let ((*fn-exit* #'(lambda (&optional (status ex-ok) &rest extra-status) 
					      (return-from into-daemonized (cons status extra-status)))))
			   (case-command daemon-command
					 ("zap" (zap-service conf-params))
					 ("stop" (stop-service conf-params))
					 ("kill" (kill-service conf-params))
					 ("restart" (let ((res (block into-daemonized-lev2
								 (let ((*fn-exit* #'(lambda (&optional (status ex-ok) &rest extra-status)
										      (return-from into-daemonized-lev2
											(cons status extra-status)))))
								   (stop-service conf-params)))))
						      (if (if (numberp res) 
							      (= ex-ok res)
							      (= ex-ok (first res)))
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
			extra-status (rest result))
		  (setq status result))
	      (when (getf conf-params :exit)
		(exit status))
	      (values status extra-status)))))
    (error (err)
      (when (eq :child *process-type*) (error err))
      (log-err "~A~%" err)
      (format t "ERROR: ~A~%" err)
      (case on-error
	(:exit-from-lisp (exit ex-general))
	(:call-error (error err))
	(:return-error err)
	(:as-ignore-errors (values nil err))))))
	  
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



