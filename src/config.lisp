(in-package :cl-user)
(defpackage quaremain.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :quaremain.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :quaremain))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common
    `(:databases ((:maindb :sqlite3 :database-name
                           ,(merge-pathnames #P"var/database.sqlite3"
                                             *application-root*)))))

(defconfig |development|
    '(:debug t))

(defconfig |test|
    '(:debug t))

(defconfig |server-production|
    '(:debug nil))

(defconfig |desktop-production|
    '(:debug nil))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
