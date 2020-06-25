(defpackage quaremain/tests/unit/models/stock/stock
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.models.stock.stock
                :sum-all-cost-per-stock
                :sum-all-calories-per-stock
                :sum-all-millilitres-per-stock
                :sum-unique-property-value-by-amount
                :coerce-cost-per-stock))
(in-package :quaremain/tests/unit/models/stock/stock)

(deftest quaremain.models.stock.stock
    (let* ((raw-result
            (sum-all-cost-per-stock
             '((:cost-per-stock 2.2 :amount 3))))
           (total-cost (getf (car raw-result) :cost-per-stock))
           (amount (getf (car raw-result) :amount))
           (food-result
            (sum-all-calories-per-stock
             '((:id 1 :amount 2 :calories-per-stock 219))))
           (calories-per-stock
            (getf (car food-result) :calories-per-stock))
           (water-result
            (sum-all-millilitres-per-stock
             '((:id 2 :amount 9 :millilitres-per-stock 9293))))
           (millilitres-per-stock
            (getf (car water-result) :millilitres-per-stock))
           (unique-property-result
            (sum-unique-property-value-by-amount
             '(:id 1 :amount 3 :fire 9823) :fire)))
      (testing ""
               (ok
                (and (= total-cost 6.6000004)
                     (= amount 3))))
      (testing ""
               (ok
                (equal (coerce-cost-per-stock
                        '(:id 1 :amount 4 :cost-per-stock 9239.84))
                       '(:id 1 :amount 4 :cost-per-stock 9239.84))))
      (testing ""
               (ok
                (= calories-per-stock 438)))
      (testing ""
               (ok
                (= millilitres-per-stock 83637)))
      (testing ""
               (ok
                (equal '(:id 1 :amount 3 :fire 29469)
                       unique-property-result)))))
