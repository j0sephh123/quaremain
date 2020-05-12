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

;;;; quaremain/tests/main - Primary unit tests.

(defpackage quaremain/tests/unit
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/unit)

;; NOTE: To run this test file, execute `(asdf:test-system :quaremain)' in your Lisp.

(deftest quaremain.utilities.string

    (testing ""
             (ok
              (eql
               (quaremain.utilities.string::string-to-keyword "fruitz")
               :fruitz))))

(deftest quaremain.models.stock.stock
    (let* ((raw-result
            (quaremain.models.stock.stock::sum-all-cost-per-package
             '((:cost-per-package 2.2 :amount 3))))
           (total-cost (getf (car raw-result) :cost-per-package))
           (amount (getf (car raw-result) :amount))
           
           (food-result
            (quaremain.models.stock.stock::sum-all-calories-per-package
             '((:id 1 :amount 2 :calories-per-package 219))))
           (calories-per-package
            (getf (car food-result) :calories-per-package))

           (water-result
            (quaremain.models.stock.stock::sum-all-millilitre-per-package
             '((:id 2 :amount 9 :millilitre-per-package 9293))))
           (millilitre-per-package
            (getf (car water-result) :millilitre-per-package))

           (unique-property-result
            (quaremain.models.stock.stock::sum-unique-property-value-by-amount
             '(:id 1 :amount 3 :fire 9823) :fire)))
      
      (testing ""
               (ok
                (and (= total-cost 6.6000004)
                     (= amount 3))))

      (testing ""
               (ok
                (equal (quaremain.models.stock.stock::coerce-cost-per-package
                        '(:id 1 :amount 4 :cost-per-package 9239.84))
                       '(:id 1 :amount 4 :cost-per-package 9239.84))))

      (testing ""
               (ok
                (= calories-per-package 438)))

      (testing ""
               (ok
                (= millilitre-per-package 83637)))

      (testing ""
               (ok
                (equal '(:id 1 :amount 3 :fire 29469)
                       unique-property-result)))))

