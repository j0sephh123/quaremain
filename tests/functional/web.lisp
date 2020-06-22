(defpackage quaremain/tests/functional/web
  (:use :cl
        :rove)
  (:import-from :quaremain
                :start
                :stop)
  (:import-from :quaremain.utilities.database
                :migrate-seeds
                :drop-tables)
  (:local-nicknames (#:server #:quaremain)))
(in-package :quaremain/tests/functional/web)

(setup
 (start :port 5001 :debug t)
 (migrate-seeds))

(teardown
 (drop-tables)
 (stop))

(defparameter +root-host+ "http://localhost:5001")

(defun get-content (root-host route)
  (dex:get (format nil "~a~a" root-host route)))

(defun get-ok? (route)
  (eql
   (nth-value 1 (get-content +root-host+ route))
   200))

(deftest experimental-route
  (testing "experimental"
           (ok
            (get-ok? "/experimental"))))

(deftest stock-lists
  (testing "food"
           (ok
            (get-ok? "/api/app/list/food")))
  (testing "water"
           (ok
            (get-ok? "/api/app/list/water")))
  (testing "medicine"
           (ok
            (get-ok? "/api/app/list/medicine")))
  (testing "weapon"
           (ok
            (get-ok? "/api/app/list/weapon"))))

(deftest show
  (princ (get-content +root-host+ "/api/app/list/show/1?stockCategory=food"))
  (testing "food"
    (ok
     (and
      (get-ok? "/api/app/list/show/1?stockCategory=food")
      (string=
       (get-content +root-host+ "/api/app/list/show/1?stockCategory=food")
       "{\"stock\":[{\"id\":1,\"name\":\"Sed neque. Sed eget lacus. Mauris\",\"description\":\"mauris id sapien. Cras dolor dolor, tempus non, lacinia at,\",\"amount\":12,\"costPerPackage\":12.02,\"caloriesPerPackage\":1200}],\"status\":200}")))))
