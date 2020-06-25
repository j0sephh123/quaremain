(defpackage quaremain.models.stock.food
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum
                :update-datum-by-id
                :row-exist-by-name?)
  (:import-from :quaremain.utilities.exception
                :row-with-same-name-already-exist-error
                :user-input-doesnt-satisfy-constraint-error)
  (:import-from :quaremain.models.stock.constraint
                :satisfies-length-constraint?
                :satisfies-integer-constraint?
                :satisfies-decimal-constraint?
                :satisfies-string-constraint?)
  (:export :create-food
           :update-food))
(in-package :quaremain.models.stock.food)

;; TODO: Differentiate between constraints errors
(defun satisfies-constraints?
    (name
     description
     amount
     cost
     calories)
  (let ((max-constraint 9999999999999))
    (unless (= (length description) 0)
      (unless (and (satisfies-length-constraint? description 20 1500)
                   (satisfies-string-constraint? description))
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-string-constraint? name)
             
             (satisfies-length-constraint? amount 1 max-constraint)
             (satisfies-integer-constraint? amount)
             
             (satisfies-length-constraint? cost 1 max-constraint)
             (satisfies-decimal-constraint? cost)
             
             (satisfies-length-constraint? calories 1 max-constraint)
             (satisfies-integer-constraint? calories))
      (error 'user-input-doesnt-satisfy-constraint-error))))

(defun create-food (food)
  (let ((name
         (get-key-value food :name))
        (description
         (get-key-value food :description))
        (amount
         (get-key-value food :amount))
        (cost-per-stock
         (get-key-value food :cost-per-stock))
        (calories-per-stock
         (get-key-value food :calories-per-stock)))

    (when (row-exist-by-name? :food name)
      (error 'row-with-same-name-already-exist-error
             :name name
             :table-name :food))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock
     calories-per-stock)

    (with-connection (db)
      (create-datum
          :food
        :name name
        :description description
        :amount amount
        :cost-per-stock cost-per-stock
        :calories-per-stock calories-per-stock))))

(defun update-food (food id)
  (let ((name
         (get-key-value food :name))
        (description
         (get-key-value food :description))
        (amount
         (get-key-value food :amount))
        (cost-per-stock
         (get-key-value food :cost-per-stock))
        (calories-per-stock
         (get-key-value food :calories-per-stock)))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock
     calories-per-stock)

    
    (with-connection (db)
      (update-datum-by-id
          :food
          id
          name
          description
          amount
          cost-per-stock
        :calories-per-stock calories-per-stock))))

