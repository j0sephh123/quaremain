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

(defpackage quaremain/tests/functional
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/functional)

(setup
 (quaremain.utilities.database::migrate-tables)
 (quaremain.utilities.database::migrate-seeds))

(teardown
 (quaremain.utilities.database::drop-tables))

(deftest database-migration-food-test
    
    (let* ((stock (quaremain.utilities.database::get-datum-from-table :food 1))
           (calories (getf stock :calories-per-package))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (description (getf stock :description)))

      (testing "cost-per-package is equal to 12.02d0"
               (ok (= cost 12.02d0)))

      (testing "calories-per-package is equal to 1200"
               (ok (= calories 1200)))

      (testing "id is equal to 1"
               (ok (= id 1)))))

(deftest database-migration-water-test
    
    (let* ((stock (quaremain.utilities.database::get-datum-from-table :water 2))
           (millilitre (getf stock :millilitre-per-package))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package))
           (amount (getf stock :amount))
           (name (getf stock :name))
           (description (getf stock :description)))

      (testing "millilitre-per-package is equal to 923"
               (ok (= millilitre 923)))

      (testing "cost-per-package is equal to 12.02d0"
               (ok (= cost 12.02d0)))

      (testing "amount is equal to 5"
               (ok (= amount 5)))

      (testing "id is equal to 2"
               (ok (= id 2)))

      (testing "name is equal to ICOLITE from Jerry's"
               (ok (string= name "ICOLITE from Jerry's")))

      (testing "description is equal to empty string"
               (ok (string= description "")))))
