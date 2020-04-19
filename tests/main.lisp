;; Quaremain - A software to manage resources for emergency times.
;; Copyright (C) 2020  Momozor <skelic3@gmail.com, momozor4@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage quaremain/tests/main
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :quaremain)' in your Lisp.

(deftest unit-tests
    (testing "(sum-all-cost-per-package '((:cost-per-package 2.2 :amount 3))) should equal to ((:cost-per-package 6.6000004 :amount 3)"
             (let* ((raw-result
                     (quaremain.web::sum-all-cost-per-package '((:cost-per-package 2.2 :amount 3))))
                    (total-cost (getf (car raw-result) :cost-per-package))
                    (amount (getf (car raw-result) :amount)))
               (ok
                (and (= total-cost 6.6000004)
                     (= amount 3)))))

  (testing "(coerce-cost-per-package '(:id 1 :amount 3 :cost-per-package 9239.84)) should equal to '(:id 1 :amount 3 :cost-per-package 9239.84)"
           (ok
            (equal (quaremain.web::coerce-cost-per-package
                    '(:id 1 :amount 4 :cost-per-package 9239.84))
                   '(:id 1 :amount 4 :cost-per-package 9239.84)))))

