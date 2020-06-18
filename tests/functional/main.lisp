(defpackage quaremain/tests/functional/main
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db)
  (:local-nicknames (#:database #:quaremain.utilities.database))
  (:local-nicknames (#:stock #:quaremain.models.stock.stock)))
(in-package :quaremain/tests/functional/main)

(setup
 (database::migrate-tables)
 (database::migrate-seeds))

(teardown
 (database::drop-tables))

;;; quaremain.utilities.database

(deftest database-migration-food-test
    
    (let* ((stock
            (with-connection (db)
              (database::get-datum-by-id :food 1)))
           (calories (getf stock :calories-per-package))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (description (getf stock :description))
           (name (getf stock :name))
           (amount (getf stock :amount)))

      (testing "name of first row"
               (ok (string= name "Sed neque. Sed eget lacus. Mauris")))

      (testing "amount of first row"
               (ok (= amount 12)))

      (testing "cost-per-package of first row"
               (ok (= cost 12.02d0)))

      (testing "calories-per-package of first row"
               (ok (= calories 1200)))

      (testing "id of first row"
               (ok (= id 1)))))

(deftest database-migration-water-test
    
    (let* ((stock
            (with-connection (db)
              (database::get-datum-by-id :water 2)))
           (millilitre (getf stock :millilitre-per-package))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (amount (getf stock :amount))
           (name (getf stock :name))
           (description (getf stock :description)))

      (testing "millilitre-per-package of second row"
               (ok (= millilitre 923)))

      (testing "cost-per-package of second row"
               (ok (= cost 12.02d0)))

      (testing "amount of second row"
               (ok (= amount 5)))

      (testing "id of second row"
               (ok (= id 2)))

      (testing "name of second row"
               (ok (string= name "ICOLITE from Jerry's")))

      (testing "description of second row"
               (ok (string= description "")))))

(deftest database-migration-medicine-test
    
    (let* ((stock
            (with-connection (db)
              (database::get-datum-by-id :medicine 1)))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (description (getf stock :description))
           (name (getf stock :name))
           (amount (getf stock :amount)))

      (testing "name of first row"
               (ok (string= name "Penicilin IoX")))

      (testing "amount of first row"
               (ok (= amount 4)))

      (testing "cost-per-package of first row"
               (ok (= cost 12.02d0)))

      (testing "id of first row"
               (ok (= id 1)))))

(deftest database-migration-weapon-test
    
    (let* ((stock
            (with-connection (db)
              (database::get-datum-by-id :weapon 2)))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (description (getf stock :description))
           (name (getf stock :name))
           (amount (getf stock :amount)))

      (testing "name of second row"
               (ok (string= name "Magnum-192")))

      (testing "amount of second row"
               (ok (= amount 2)))

      (testing "cost-per-package of second row"
               (ok (= cost 900.78d0)))

      (testing "id of second row"
               (ok (= id 2)))))

(deftest get-all-datum
    (with-connection (db)
      (let* ((results
              (database::get-all-datum :weapon))
             (first-row
              (first results))
             (second-row
              (second results)))

        (testing "first row of weapon"
                 (ok
                  (equal
                   first-row
                   '(:id 1
                     :name "AK-47"
                     :description "mauris id sapien. Cras dolor dolor, tempus non, lacinia at,"
                     :amount 1
                     :cost-per-package 1200.0d0)
                   )))

        (testing "second row of weapon"
                 (ok
                  (equal
                   second-row
                   '(:id 2
                     :name "Magnum-192"
                     :description "Poweful shockburst"
                     :amount 2
                     :cost-per-package 900.78d0)))))))

(deftest get-datum-by-id
    (with-connection (db)
      (let* ((result
              (database::get-datum-by-id
               :medicine 1))
             (amount (getf result :amount)))

        (testing "amount of first row of medicine"
                 (ok
                  (= amount
                     4))))))

(deftest get-datum-by-name
    (with-connection (db)
      (let* ((result
              (database::get-datum-by-name
               :food
               "Cashews from Jerry's"))
             (amount (getf result :amount)))

        (testing "food by Cashews from Jerry's"
                 (ok
                  (= amount
                     5))))))

(deftest row-exist-by-id?
    (with-connection (db)
      (let* ((food-result
              (database::row-exist-by-id? :food 1))
             (weapon-result
              (database::row-exist-by-id? :weapon 2)))
        
        (testing "first row of food"
                 (ok food-result))

        (testing "second row of food"
                 (ok weapon-result)))))

(deftest row-exist-by-name?
    (with-connection (db)
      (let* ((food-result
              (database::row-exist-by-name?
               :food
               "Sed neque. Sed eget lacus. Mauris"))
             (weapon-result
              (database::row-exist-by-name?
               :weapon
               "Magnum-192")))

        (testing "by name Sed neque. Sed eget lacus. Mauris"
                 (ok food-result))

        (testing "by name Magnum-192"
                 (ok weapon-result)))))

(deftest create-new-row
    (with-connection (db)
      (database::create-datum
       :food
       :name "Jaguar"
       :description "Food"
       :amount 3
       :cost-per-package 12.0
       :calories-per-package 1492))

  (with-connection (db)
    (let* ((food-result
            (database::get-datum-by-id :food 3))
           (food-description
            (getf food-result :description)))

      (testing "food"
               (ok
                (string=
                 food-description
                 "Food"))))))

(deftest update-datum-by-id
    (with-connection (db)

      (database::update-datum-by-id
       :water
       2
       "Fuvi"
       "Slurp"
       223
       12.0
       :millilitre-per-package 1000)
      
      (database::update-datum-by-id
       :medicine
       1
       "Cough-ed"
       "Healer"
       923
       14.0)

      (let* ((water-result
              (database::get-datum-by-id :water 2))
             (water-millilitre
              (getf water-result :millilitre-per-package))
             
             (medicine-result
              (database::get-datum-by-id :medicine 1))
             (medicine-amount
              (getf medicine-result :amount)))

        (testing "water millilitre-per-package of second row"
                 (ok
                  (=
                   water-millilitre
                   1000)))

        (testing "medicine amount of first row"
                 (ok
                  (=
                   medicine-amount
                   923))))))

(deftest delete-datum-by-id
    (with-connection (db)

      (database::delete-datum-by-id :water 1)
      (database::delete-datum-by-id :medicine 1)
      (database::delete-datum-by-id :medicine 2)

      (testing "water first row"
               (ng
                (quaremain.utilities.database::get-datum-by-id :water 1)))

      (testing "water second row"
               (ok
                (quaremain.utilities.database::get-datum-by-id :water 2)))

      (testing "medicine first row"
               (ng
                (quaremain.utilities.database::get-datum-by-id :medicine 1)))

      (testing "medicine second row"
               (ng
                (quaremain.utilities.database::get-datum-by-id :medicine 2)))

      ;; reset database for next package testing
      (database::drop-tables)
      (database::migrate-tables)
      (database::migrate-seeds)))

;;; quaremain.models.stock.stock
(deftest create-stock  
    (stock::create-stock
     '((:stock-category . "food")
       (:name . "RANMIZ")
       (:description . "slurpy")
       (:amount . 24)
       (:cost-per-package . 27.02d20)
       (:calories-per-package . 923)))
  (stock::create-stock
   '((:stock-category . "water")
     (:name . "Fuviz")
     (:description . "refreshing")
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

(deftest calculate-total-survival-days
    (let* ((result
            (stock::calculate-total-survival-days
             (stock::get-total-food-calories)
             (stock::get-total-water-millilitre)))

           (result-two
            (stock::calculate-total-survival-days
             (- (stock::get-total-food-calories)
                3000)
             (- (stock::get-total-water-millilitre)
                4000))))

      (testing ""
               (ok
                (= result
                   10)))

      (testing ""
               (ok
                (= result-two
                   8)))))

(deftest get-total-survival-days
    (let ((result
           (stock::get-total-survival-days)))

      (testing ""
               (ok
                (= result
                   10)))))

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
         (:name . "Firenzo")
         (:description . "Ranimoul")
         (:amount . 2)
         (:cost-per-package . 198.02d0)))
      (stock::update-stock-by-id
       `((:stock-category . "water")
         (:id . 1)
         (:name . "Yaml")
         (:description . "Farapa")
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
