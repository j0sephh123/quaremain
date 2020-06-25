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
                :satisfies-length-constraint?)
  (:export :create-medicine
           :update-medicine))
(in-package :quaremain.models.stock.medicine)

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
    
    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-stock 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))
    
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
    
    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-stock 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))
    
    (with-connection (db)
      (update-datum-by-id
       :medicine
       id
       name
       description
       amount
       cost-per-stock))))
