(defpackage quaremain.models.stock.medicine
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
  (:export :create-medicine
           :update-medicine))
(in-package :quaremain.models.stock.medicine)

(defun satisfies-constraints?
    (name
     description
     amount
     cost)
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
             (satisfies-decimal-constraint? cost))
      (error 'user-input-doesnt-satisfy-constraint-error))))

(defun create-medicine (medicine)
  (let ((name
         (get-key-value medicine :name))
        (description
         (get-key-value medicine :description))
        (amount
         (get-key-value medicine :amount))
        (cost-per-stock
         (get-key-value medicine :cost-per-stock)))

    (when (row-exist-by-name? :medicine name)
      (error 'row-with-same-name-already-exist-error
             :name name
             :table-name :medicine))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock)
    
    (with-connection (db)
      (create-datum
       :medicine 
       :name name
       :description description
       :amount amount
       :cost-per-stock cost-per-stock))))

(defun update-medicine (medicine id)
  (let ((name
         (get-key-value medicine :name))
        (description
         (get-key-value medicine :description))
        (amount
         (get-key-value medicine :amount))
        (cost-per-stock
         (get-key-value medicine :cost-per-stock)))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock)
    
    (with-connection (db)
      (update-datum-by-id
       :medicine
       id
       name
       description
       amount
       cost-per-stock))))
