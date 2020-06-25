(defpackage quaremain/tests/functional/models/stock/survival
  (:use :cl :rove)
  (:import-from :quaremain.models.stock.survival
                :calculate-total-survival-days
                :get-total-survival-days)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :migrate-tables
                :migrate-seeds
                :drop-tables)
  (:local-nicknames (#:stock #:quaremain.models.stock.stock)))
(in-package :quaremain/tests/functional/models/stock/survival)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

;; TODO: Move this test to unit.
(deftest calculate-total-survival-days
  (let* ((result
          (calculate-total-survival-days
           (stock::get-total-food-calories)
           (stock::get-total-water-millilitres)))

         (result-two
          (calculate-total-survival-days
           (- (stock::get-total-food-calories)
              3000)
           (- (stock::get-total-water-millilitres)
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
         (get-total-survival-days)))
    (testing ""
             (ok
              (= result
                 10)))))
