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
                :satisfies-length-constraint?
                :satisfies-integer-constraint?
                :satisfies-decimal-constraint?
                :satisfies-string-constraint?)
  (:export :create-weapon
           :update-weapon))
(in-package :quaremain.models.stock.weapon)

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

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock)
    
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

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock)
    
    (with-connection (db)
      (update-datum-by-id
          :weapon
          id
          name
          description
          amount
          cost-per-stock))))
