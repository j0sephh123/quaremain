(defpackage quaremain/tests/functional/models/stock
  (:use :cl
        :rove)
  (:import-from :quaremain.utilities.database
                :migrate-tables
                :migrate-seeds
                :drop-tables
                :with-connection
                :db)
  (:local-nicknames (#:database #:quaremain.utilities.database))
  (:local-nicknames (#:stock #:quaremain.models.stock.stock)))
(in-package :quaremain/tests/functional/models/stock)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

(deftest create-stock  
    (stock::create-stock
     '((:stock-category . "food")
       (:name . "RANMIZ02030")
       (:description . "slurpy727372hhawhebawhsaws")
       (:amount . 24)
       (:cost-per-package . 27.02d0)
       (:calories-per-package . 923)))
  (stock::create-stock
   '((:stock-category . "water")
     (:name . "Fuvizz")
     (:description . "refreshingawheawhehjahjehawehahwjehjahwje")
     (:amount . 5)
     (:cost-per-package . 3.02d0)
     (:millilitre-per-package . 1100)))
  (stock:create-stock
   '((:stock-category . "medicine")
     (:name . "Paremw")
     (:description . "")
     (:amount . 9)
     (:cost-per-package . 9.02d0)))
  (stock:create-stock
   '((:stock-category . "weapon")
     (:name . "JRYZ")
     (:description . "")
     (:amount . 4)
     (:cost-per-package . 2300.00d0)))
  (with-connection (db)
    (let* ((food-result
            (database::get-datum-by-id :food 3))
           (food-amount
            (getf food-result :amount))
           (food-calories
            (getf food-result :calories-per-package))
           (water-result
            (database::get-datum-by-id :water 3))
           (water-amount
            (getf water-result :amount))
           (water-millilitre
            (getf water-result :millilitre-per-package))
           (water-cost
            (getf water-result :cost-per-package))
           (medicine-result
            (database::get-datum-by-id :medicine 3))
           (medicine-amount
            (getf medicine-result :amount))
           (weapon-result
            (database::get-datum-by-id :weapon 3))
           (weapon-name
            (getf weapon-result :name)))
      (testing "third row of food"
               (ok
                (= food-amount
                   24)))
      (testing "third row of food"
               (ok
                (= food-calories
                   923)))
      (testing "third row of water"
               (ok
                (= water-amount
                   5)))
      (testing "third row of water"
               (ok
                (= water-millilitre
                   1100)))
      (testing "third row of water"
               (ok
                (= water-cost
                   3.02d0)))
      (testing "third row of medicine"
               (ok
                (= medicine-amount
                   9)))
      (testing "third row of weapon"
               (ok
                (string= weapon-name
                         "JRYZ")))
      (database::delete-datum-by-id
       :food 3)
      (database::delete-datum-by-id
       :water 3)
      (database::delete-datum-by-id
       :medicine 3)
      (database::delete-datum-by-id
       :weapon 3))))

(deftest get-stocks-sum
    (let* ((food-result
            (stock::get-stocks-sum
             :food))
           (medicine-result
            (stock::get-stocks-sum
             :medicine)))

      (testing "food"
               (ok
                (equal
                 food-result
                 '((:ID 1 :NAME "Sed neque. Sed eget lacus. Mauris"
                    :DESCRIPTION
                    "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                    :AMOUNT 12 :COST-PER-PACKAGE 144.24 :CALORIES-PER-PACKAGE
                    14400)
                   (:ID 2 :NAME "Cashews from Jerry's" :DESCRIPTION "" :AMOUNT
                    5 :COST-PER-PACKAGE 60.1 :CALORIES-PER-PACKAGE 1000))
                 )))

      (testing "medicine"
               (ok
                (equal
                 medicine-result
                 '((:ID 1 :NAME "Penicilin IoX" :DESCRIPTION
                    "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                    :AMOUNT 4 :COST-PER-PACKAGE 48.08)
                   (:ID 2 :NAME "Paracetamol" :DESCRIPTION
                    "For fever (ASAP)" :AMOUNT 5 :COST-PER-PACKAGE 15.1))
                 )))))

(deftest get-coerced-stock-cost-by-id
    (let* ((food-result
            (stock::get-coerced-stock-cost-by-id
             "food"
             1))
           (food-cost
            (getf food-result :cost-per-package)))
      
      (testing "first row of food"
               (ok
                (=
                 food-cost
                 12.02)))))

(deftest get-total-unique-property-stock-value-sum
    (with-connection (db)
      
      (let* ((result
              (stock::get-total-unique-property-stock-value-sum
               :calories-per-package
               (stock::sum-all-calories-per-stock
                (database::get-all-datum :food)))))

        (testing "food"
                 (ok
                  (= result
                     15400))))))


(deftest get-total-food-calories
    (let* ((result
            (stock::get-total-food-calories)))
      
      (testing ""
               (ok
                (= result
                   15400)))))

(deftest get-total-water-millilitre
  (let* ((result
          (stock::get-total-water-millilitre)))

    (testing ""
             (ok
              (= result
                 22615)))))

(deftest get-all-stocks
  (let ((result
         (stock::get-all-stocks)))

    (testing ""
             (ok
              (equal result

                     '(:FOOD
                       ((:ID 1 :NAME "Sed neque. Sed eget lacus. Mauris" :DESCRIPTION
                         "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                         :AMOUNT 12 :COST-PER-PACKAGE 12.02d0 :CALORIES-PER-PACKAGE
                         1200)
                        (:ID 2 :NAME "Cashews from Jerry's" :DESCRIPTION "" :AMOUNT 5
                         :COST-PER-PACKAGE 12.02d0 :CALORIES-PER-PACKAGE 200))
                       :WATER
                       ((:ID 1 :NAME "Tasty drinking water" :DESCRIPTION
                         "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                         :AMOUNT 12 :COST-PER-PACKAGE 12.02d0 :MILLILITRE-PER-PACKAGE
                         1500)
                        (:ID 2 :NAME "ICOLITE from Jerry's" :DESCRIPTION "" :AMOUNT 5
                         :COST-PER-PACKAGE 12.02d0 :MILLILITRE-PER-PACKAGE 923))
                       :MEDICINE
                       ((:ID 1 :NAME "Penicilin IoX" :DESCRIPTION
                         "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                         :AMOUNT 4 :COST-PER-PACKAGE 12.02d0)
                        (:ID 2 :NAME "Paracetamol" :DESCRIPTION "For fever (ASAP)"
                         :AMOUNT 5 :COST-PER-PACKAGE 3.02d0))
                       :WEAPON
                       ((:ID 1 :NAME "AK-47" :DESCRIPTION
                         "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                         :AMOUNT 1 :COST-PER-PACKAGE 1200.0d0)
                        (:ID 2 :NAME "Magnum-192" :DESCRIPTION "Poweful shockburst"
                         :AMOUNT 2 :COST-PER-PACKAGE 900.78d0))))))))


(deftest update-stock-by-id
    (with-connection (db)    
      (stock::update-stock-by-id
       `((:stock-category . "weapon")
         (:id . 1)
         (:name . "Firenzozzzz")
         (:description . "")
         (:amount . 2)
         (:cost-per-package . 198.02d0)))
      (stock::update-stock-by-id
       `((:stock-category . "water")
         (:id . 1)
         (:name . "Yamlzzzww")
         (:description . "")
         (:amount . 5)
         (:cost-per-package . 192.02d0)
         (:millilitre-per-package . 912)))
      (let* ((weapon-result
              (database::get-datum-by-id
               :weapon
               1))
             (weapon-cost
              (getf weapon-result :cost-per-package))
             (water-result
              (database::get-datum-by-id
               :water
               1))
             (water-amount
              (getf water-result :amount))
             (water-millilitre
              (getf water-result :millilitre-per-package)))
        (testing "weapon first row"
                 (ok
                  (=
                   weapon-cost
                   198.02d0)))
        (testing "water first row"
                 (ok
                  (= water-amount
                     5))
                 (ok
                  (= water-millilitre
                     912))))))

(deftest delete-stock-by-id
    (stock::delete-stock-by-id
     "weapon"
     2)

  (let ((weapon-result
         (database::row-exist-by-id?
          :weapon 2)))

    (testing "weapon second row"
             (ng weapon-result))))
