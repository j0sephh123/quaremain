;;;; Quaremain - A software to manage resources for emergency times.
;;;; Copyright (C) 2020  Momozor <skelic3@gmail.com, momozor4@gmail.com>

;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; quaremain/tests/main - Primary functional tests.

(defpackage quaremain/tests/functional/main
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db))
(in-package :quaremain/tests/functional/main)

(setup
 (quaremain.utilities.database::migrate-tables)
 (quaremain.utilities.database::migrate-seeds))

(teardown
 (quaremain.utilities.database::drop-tables))

(deftest database-migration-food-test
  
  (let* ((stock
          (with-connection (db)
            (quaremain.utilities.database::get-datum-by-id :food 1)))
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
            (quaremain.utilities.database::get-datum-by-id :water 2)))
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
            (quaremain.utilities.database::get-datum-by-id :medicine 1)))
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
            (quaremain.utilities.database::get-datum-by-id :weapon 2)))
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
            (quaremain.utilities.database::get-all-datum :weapon))
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
              (quaremain.utilities.database::get-datum-by-id
               :medicine 1))
             (amount (getf result :amount)))

        (testing "amount of first row of medicine"
                 (ok
                  (= amount
                     4))))))

(deftest get-datum-by-name
    (with-connection (db)
      (let* ((result
              (quaremain.utilities.database::get-datum-by-name
               :food
               "Cashews from Jerry's"))
             (amount (getf result :amount)))

        (testing "food by Cashews from Jerry's"
                 (ok
                  (= amount
                     5))))))

(deftest row-exist-by-id?
    (with-connection (db)
      (let ((result
             (quaremain.utilities.database::row-exist-by-id?
              :food 1)))

        (testing "first row of food"
                 (ok result)))))

(deftest row-exist-by-name?
    (with-connection (db)
      (let ((result
             (quaremain.utilities.database::row-exist-by-name?
              :weapon
              "Magnum-192")))

        (testing "weapon by Magnum-192"
                 (ok result)))))

(deftest get-datum-by-name
  (with-connection (db)
    (let* ((result
            (quaremain.utilities.database::get-datum-by-name
             :food
             "Cashews from Jerry's"))
           (amount (getf result :amount)))

      (testing "amount of first row of food"
               (ok
                (= amount
                   5))))))

(deftest row-exist-by-id?
  (with-connection (db)
    (let* ((food-result
            (quaremain.utilities.database::row-exist-by-id? :food 1))
           (weapon-result
            (quaremain.utilities.database::row-exist-by-id? :weapon 2)))
      
      (testing "first row of food"
               (ok food-result))

      (testing "second row of food"
               (ok weapon-result)))))

(deftest row-exist-by-name?
  (with-connection (db)
    (let* ((food-result
            (quaremain.utilities.database::row-exist-by-name?
             :food
             "Sed neque. Sed eget lacus. Mauris"))
           (weapon-result
            (quaremain.utilities.database::row-exist-by-name?
             :weapon
             "Magnum-192")))

      (testing "by name Sed neque. Sed eget lacus. Mauris"
               (ok food-result))

      (testing "by name Magnum-192"
               (ok weapon-result)))))

(deftest create-new-row
    (with-connection (db)
      (quaremain.utilities.database::create-datum
       :food
       :name "Jaguar"
       :description "Food"
       :amount 3
       :cost-per-package 12.0
       :calories-per-package 1492))

  (with-connection (db)
    (let* ((food-result
            (quaremain.utilities.database::get-datum-by-id :food 3))
           (food-description
            (getf food-result :description)))

      (testing "food"
               (ok
                (string=
                 food-description
                 "Food"))))))

(deftest update-datum
  (with-connection (db)

    (quaremain.utilities.database::update-datum :water
        2
        "Fuvi"
        "Slurp"
        223
        12.0
      :millilitre-per-package 1000)
    
    (quaremain.utilities.database::update-datum :medicine
        1
        "Cough-ed"
        "Healer"
        923
        14.0)

    (let* ((water-result
            (quaremain.utilities.database::get-datum-by-id :water 2))
           (water-millilitre
            (getf water-result :millilitre-per-package))
           
           (medicine-result
            (quaremain.utilities.database::get-datum-by-id :medicine 1))
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

(deftest delete-datum
  (with-connection (db)

    (quaremain.utilities.database::delete-datum :water 1)
    (quaremain.utilities.database::delete-datum :medicine 1)
    (quaremain.utilities.database::delete-datum :medicine 2)

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
              (quaremain.utilities.database::get-datum-by-id :medicine 2)))))
