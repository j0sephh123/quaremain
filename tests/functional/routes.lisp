(defpackage quaremain/tests/functional/routes
  (:use :cl
        :quaremain
        :rove)
  (:local-nicknames (#:server #:quaremain))
  (:local-nicknames (#:routes #:quaremain.web))
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/routes)

(setup
  (server:start :port 5000 :debug t)
  (database::migrate-seeds))

(teardown
  (database::drop-tables)
  (server:stop))

(defparameter +root-host+ "http://localhost:5000")

(defun get-content (root-host route)
  (dex:get (format nil "~a~a" root-host route)))

(defun get-ok (route)
  (eql
   (nth-value 1 (get-content +root-host+ route))
   200))

(deftest index-or-experimental-route
  (testing ""
    (ok
     (get-ok "/"))))
