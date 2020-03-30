(in-package :cl-user)
(defpackage quaremain.web
  (:use :cl
        :caveman2
        :quaremain.config
        :quaremain.view
        :quaremain.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :quaremain.web)

;; For @route annotation.
(syntax:use-syntax :annot)

;; Application.

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;; Routing rules.

(defroute "/" ()
  (render #p"index.html"))

(defroute "/auth/login-page" ()
  (render #p"auth/login-page.html"))

(defroute ("/auth/logout" :method :POST) ()
  (render #p"index.html"))

(defroute "/about" ()
  (render #p"about.html"))

;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
