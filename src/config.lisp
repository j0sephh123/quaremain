(in-package :cl-user)
(defpackage quaremain.config
  (:use :cl)
  (:export :*static-directory*
           :*template-directory*))
(in-package :quaremain.config)

(defparameter *static-directory* (pathname "static/"))
(defparameter *template-directory* (pathname "templates/"))
