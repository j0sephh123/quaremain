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

(deftest database-migration-food
    
    (let* ((stock (quaremain.utilities.database::get-datum-from-table :food 1))
           (calories (getf stock :calories-per-package))
           (id (getf stock :id))
           (cost (getf stock :cost-per-package)))

      (testing "cost-per-package is equal to 13.0d"
               (ok (= cost 12.02d0)))

      (testing "calories-per-package is equal to 500"
               (ok (= calories 1200)))

      (testing "id is equal to 1"
               (ok (= id 1)))))
