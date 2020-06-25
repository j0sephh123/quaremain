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
                :satisfies-length-constraint?)
  (:export :create-food
           :update-food))
(in-package :quaremain.models.stock.food)

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

    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-stock 1 9999999999999)
             (satisfies-length-constraint? calories-per-stock 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))

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

    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-stock 1 9999999999999)
             (satisfies-length-constraint? calories-per-stock 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))
    
    (with-connection (db)
      (update-datum-by-id
       :food
       id
       name
       description
       amount
       cost-per-stock
       :calories-per-stock calories-per-stock))))
