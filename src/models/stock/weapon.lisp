(defpackage quaremain.models.stock.weapon
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
  (:export :create-weapon
           :update-weapon))
(in-package :quaremain.models.stock.weapon)

(defun create-weapon (weapon)
  (let ((name
         (get-key-value weapon :name))
        (description
         (get-key-value weapon :description))
        (amount
         (get-key-value weapon :amount))
        (cost-per-stock
         (get-key-value weapon :cost-per-stock)))

    (when (row-exist-by-name? :weapon name)
      (error 'row-with-same-name-already-exist-error
             :name name
             :table-name :weapon))

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
       :weapon
       :name name
       :description description
       :amount amount
       :cost-per-stock cost-per-stock))))

(defun update-weapon (weapon id)
  (let ((name
         (get-key-value weapon :name))
        (description
         (get-key-value weapon :description))
        (amount
         (get-key-value weapon :amount))
        (cost-per-stock
         (get-key-value weapon :cost-per-stock)))

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
       :weapon
       id
       name
       description
       amount
       cost-per-stock))))
