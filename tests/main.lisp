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

(defpackage quaremain/tests/main
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :quaremain)' in your Lisp.

(deftest sum-all-cost-per-package
  (testing "(sum-all-cost-per-package '((:cost-per-package 2.2 :amount 3))) should equal to ((:cost-per-package 6.6000004 :amount 3)"
           (let* ((raw-result
                   (quaremain.models.stock.stock::sum-all-cost-per-package '((:cost-per-package 2.2 :amount 3))))
                  (total-cost (getf (car raw-result) :cost-per-package))
                  (amount (getf (car raw-result) :amount)))
             (ok
              (and (= total-cost 6.6000004)
                   (= amount 3))))))

(deftest coerce-cost-per-package
  (testing "(coerce-cost-per-package '(:id 1 :amount 3 :cost-per-package 9239.84)) should equal to '(:id 1 :amount 3 :cost-per-package 9239.84)"
           (ok
            (equal (quaremain.models.stock.stock::coerce-cost-per-package
                    '(:id 1 :amount 4 :cost-per-package 9239.84))
                   '(:id 1 :amount 4 :cost-per-package 9239.84)))))

(deftest sum-all-calories-per-package
  (testing "(sum-all-calories-per-package '((:id 1 :amount 2 :calories-per-package 219))) first list :calories-per-package value should result to 438"
           (let* ((raw-result
                   (quaremain.models.stock.stock::sum-all-calories-per-package
                    '((:id 1 :amount 2 :calories-per-package 219))))
                  (calories-per-package
                   (getf (car raw-result)
                         :calories-per-package)))
             (ok
              (= calories-per-package 438)))))


(deftest string-to-keyword
  (testing "(string-to-keyword \"fruitz\") should equal to :fruitz"
           (ok
            (eql (quaremain.utilities.string::string-to-keyword "fruitz")
                 :fruitz))))
