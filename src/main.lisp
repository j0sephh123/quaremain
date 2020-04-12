(in-package :cl-user)
(defpackage quaremain
  (:use :cl)
  (:import-from :quaremain.config
                :config
                :productionp
                :*static-directory*)
  (:import-from :clack
                :clackup)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :quaremain.web
                :migrate-models
                :*web*)

  (:export :start
           :stop
           :main))
(in-package :quaremain)

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (migrate-models)
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup 
               (builder
                (:static
                 :path (lambda (path)
                         (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
                             path
                             nil))
                 :root *static-directory*)
                (if (productionp)
                    nil
                    :accesslog)
                (if (getf (config) :error-log)
                    `(:backtrace
                      :output ,(getf (config) :error-log))
                    nil)
                :session
                (if (productionp)
                    nil
                    (lambda (app)
                      (lambda (env)
                        (let ((datafly:*trace-sql* t))
                          (funcall app env)))))
                *web*) args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun main (&key (port 5000))
  (start :port (let ((env-port (uiop:getenv "PORT")))
                 (if (null env-port)
                     port
                     (parse-integer env-port))))
  ;; with bordeaux-threads
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    (#+sbcl sb-sys:interactive-interrupt
      #+ccl  ccl:interrupt-signal-condition
      #+clisp system::simple-interrupt-condition
      #+ecl ext:interactive-interrupt
      #+allegro excl:interrupt-signal
      () (progn
           (format *error-output* "Aborting.~&")
           (stop)
           (uiop:quit 1))))) ;; portable exit, included in ASDF, already loaded.
;; for others, unhandled errors (we might want to do the same).
