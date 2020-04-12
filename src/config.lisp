(in-package :cl-user)
(defpackage quaremain.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :absolute-path
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :quaremain.config)

(setf (config-env-var) "APP_ENV")

(defparameter *static-directory* (pathname "static/"))
(defparameter *template-directory* (pathname "templates/"))

(defconfig |development|
    '(:debug t))

(defconfig |test|
    '(:debug t))

(defconfig |production|
    '(:debug nil))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
