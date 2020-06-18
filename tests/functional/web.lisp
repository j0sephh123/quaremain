(defpackage quaremain/tests/functional/web
  (:use :cl
        :quaremain
        :rove)
  (:local-nicknames (#:server #:quaremain))
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/web)

(setup
 (server:start :port 5001 :debug t)
 (database::migrate-seeds))

(teardown
  (database::drop-tables)
  (server:stop))

(defparameter +root-host+ "http://localhost:5001")

(defun get-content (root-host route)
  (dex:get (format nil "~a~a" root-host route)))

(defun get-ok (route)
  (eql
   (nth-value 1 (get-content +root-host+ route))
   200))

(deftest index-and-experimental-route
  (testing "index"
    (ok
     (get-ok "/")))
  (testing "experimental"
    (ok
     (get-ok "/experimental"))))

(deftest stock-lists
  (testing "food"
    (ok
     (get-ok "/api/app/list/food")))
  (testing "water"
    (ok
     (get-ok "/api/app/list/water")))
  (testing "medicine"
    (ok
     (get-ok "/api/app/list/medicine")))
  (testing "weapon"
    (ok
     (get-ok "/api/app/list/weapon"))))
