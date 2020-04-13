(ql:quickload :quaremain :silent t)

(defpackage quaremain.app
  (:use :cl)
  (:import-from :lack.builder
                :builder)
  (:import-from :ppcre
                :scan
                :regex-replace)
  (:import-from :quaremain.web
                :*web*)
  (:import-from :quaremain.config
                :*static-directory*))
(in-package :quaremain.app)

(builder
 (:static
  :path (lambda (path)
          (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)" path)
              path
              nil))
  :root *static-directory*)
 :session
 *web*)
