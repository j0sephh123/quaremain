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
 (sleep 2)
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
              (string=
               (get-content
                +root-host+
                "/api/app/list/create?stockCategory=food&name=firenzoautomato&description=qwertyuiopasdfgghjklzxcvbnm&costPerPackage=14.04&stockAmount=923&caloriesPerPackage=899&millilitrePerPackage=")
               "{\"status\":200,\"registeredStock\":{\"stockCategory\":\"food\",\"name\":\"firenzoautomato\",\"description\":\"qwertyuiopasdfgghjklzxcvbnm\",\"amount\":\"923\",\"costPerPackage\":\"14.04\",\"caloriesPerPackage\":\"899\",\"millilitrePerPackage\":\"\"}}")))
  (testing "water"
           (ok
            (string=
             (get-content
              +root-host+
              "/api/app/list/create?stockCategory=water&name=fulizometazo&description=&costPerPackage=2.02&stockAmount=14&millilitrePerPackage=1200&caloriesPerPackage=")
             "{\"status\":200,\"registeredStock\":{\"stockCategory\":\"water\",\"name\":\"fulizometazo\",\"description\":\"\",\"amount\":\"14\",\"costPerPackage\":\"2.02\",\"caloriesPerPackage\":\"\",\"millilitrePerPackage\":\"1200\"}}")))
  (testing "weapon with name fire"
           (ok
            (string=
             (get-content
              +root-host+
              "/api/app/list/create?stockCategory=weapon&name=fire&description=&costPerPackage=2.02&stockAmount=14&millilitrePerPackage=&caloriesPerPackage=")
             "{\"error\":\"User input doesn't satisfy constraint!\",\"status\":404}"))))

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
                   (get-ok?
                    "/api/app/list/update/1?stockCategory=food&name=tacos&description=&stockAmount=4&costPerPackage=3.20&caloriesPerPackage=800&millilitrePerPackage=")
                   (string=
                    (get-content
                     +root-host+
                     "/api/app/list/update/1?stockCategory=food&name=tacosnawne&description=jkajwkeawhjehawehawhjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjajehwaje&stockAmount=4&costPerPackage=3.20&caloriesPerPackage=800&millilitrePerPackage=")
                    "{\"status\":200}")))))

      (let* ((food-result
              (get-datum-by-id :food 1))
             (food-amount
              (getf food-result :amount)))
        (testing "food (row 1) after update"
                 (ok
                  (= food-amount
                     4))))

      (testing "medicine update with ozu as description"
               (ok
                (string=
                 (get-content
                  +root-host+
                  "/api/app/list/update/1?stockCategory=medicine&name=amoeba&description=ozu&stockAmount=4&costPerPackage=3.20&caloriesPerPackage=&millilitrePerPackage=")
                 "{\"error\":\"User input doesn't satisfy constraint!\",\"status\":404}")))))

(deftest delete-stock
  (testing "food-delete"
    (ok
     (string=
      (get-content
       +root-host+
       "/api/app/list/delete/3?stockCategory=food")
      "{\"status\":200}")))

  (testing "food-after-delete"
    (ok
     (string=
      (get-content
       +root-host+
       "/api/app/list/delete/3?stockCategory=food")
      "{\"error\":\"Item doesn't exist to be deleted!\",\"status\":404}"))))

(deftest total-survival-days
  (migrate-seeds)
  (migrate-seeds)
  (testing "empty"
           (ok
            (string=
             (get-content +root-host+ "/api/app/list/total-survival-days")
             "{\"totalSurvivalDays\":23,\"status\":200,\"survivalAlertType\":\"info\"}")))

  (migrate-seeds)
  (migrate-seeds)
  (testing "full"
           (ok
            (string=
             (get-content +root-host+ "/api/app/list/total-survival-days")
              "{\"totalSurvivalDays\":43,\"status\":200,\"survivalAlertType\":\"success\"}"))))

(deftest reset-database
  (testing "reset"
    (ok
     (get-ok? "/api/app/list/reset-database"))))
