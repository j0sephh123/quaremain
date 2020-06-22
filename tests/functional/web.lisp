(defpackage quaremain/tests/functional/web
  (:use :cl
        :rove)
  (:import-from :quaremain
                :start
                :stop)
  (:import-from :quaremain.utilities.database
                :migrate-seeds
                :drop-tables
                :with-connection
                :db
                :get-datum-by-id))
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
  (testing "food"
    (ok
     (and
      (get-ok? "/api/app/list/show/1?stockCategory=food")
      (string=
       (get-content +root-host+ "/api/app/list/show/1?stockCategory=food")
       "{\"stock\":[{\"id\":1,\"name\":\"Sed neque. Sed eget lacus. Mauris\",\"description\":\"mauris id sapien. Cras dolor dolor, tempus non, lacinia at,\",\"amount\":12,\"costPerPackage\":12.02,\"caloriesPerPackage\":1200}],\"status\":200}")))))

(deftest create-stock
  (testing "food"
    (ok
     (and
      (get-ok? "/api/app/list/create?stockCategory=food&name=fire&description=owo&costPerPackage=14.04&stockAmount=923&caloriesPerPackage=899&millilitrePerPackage=")
      (string=
       (get-content
        +root-host+
        "/api/app/list/create?stockCategory=food&name=fire&description=owo&costPerPackage=14.04&stockAmount=923&caloriesPerPackage=899&millilitrePerPackage=")

       "{\"status\":200,\"registeredStock\":{\"stockCategory\":\"food\",\"name\":\"fire\",\"description\":\"owo\",\"amount\":\"923\",\"costPerPackage\":\"14.04\",\"caloriesPerPackage\":\"899\",\"millilitrePerPackage\":\"\"}}")))))

(deftest update-stock
  (with-connection (db)
    (let* ((food-result
            (get-datum-by-id :food 1))
           (food-amount
            (getf food-result :amount)))
      (testing "food (row 1) before update"
        (ok
         (= food-amount
            12)))
      (testing "food-access-before-update"
        (ok
         (and
          (get-ok? "/api/app/list/update/1?stockCategory=food&name=tacos&description=&stockAmount=4&costPerPackage=3.20&caloriesPerPackage=800&millilitrePerPackage=")
          (string=
           (get-content
            +root-host+
            "/api/app/list/update/1?stockCategory=food&name=tacos&description=&stockAmount=4&costPerPackage=3.20&caloriesPerPackage=800&millilitrePerPackage=")
           "{\"status\":200}")))))

    (let* ((food-result
            (get-datum-by-id :food 1))
           (food-amount
            (getf food-result :amount)))
      (testing "food (row 1) after -update"
        (ok
         (= food-amount
            4))))))
