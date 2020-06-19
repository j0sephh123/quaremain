;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain
  (:documentation "Server initialization handler.")
  (:use :cl)
  (:import-from :quaremain.utilities.config
                :+static-directory+)
  (:import-from :clack
                :clackup)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :quaremain.utilities.database
                :migrate-tables)
  (:import-from :quaremain.web
                :*web*)

  (:export :start
           :stop
           :main))
(in-package :quaremain)

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (migrate-tables)
  (log:info "Proceeding to launch the local server..")
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply
         #'clackup
         (builder
          (:static
           :path
           (lambda (path)
             (if (ppcre:scan "^(?:/img/|/fonts/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
                 path
                 nil))
           :root +static-directory+)
          :session
          *web*)
         args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defun main (&key (port 5000))
  (start :port (let ((env-port (uiop:getenv "QUAREMAIN_PORT")))
                 (if (null env-port)
                     port
                     (parse-integer env-port))))
  (handler-case (bt:join-thread
                 (find-if
                  (lambda (thread)
                    (search "hunchentoot"
                            (bt:thread-name thread)))
                  (bt:all-threads)))    
    (#+sbcl sb-sys:interactive-interrupt
      () (progn
           (log:info "Aborting")
           (stop)
           (uiop:quit)))))

