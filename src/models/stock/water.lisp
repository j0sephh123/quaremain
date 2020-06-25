(defpackage quaremain.models.stock.water
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
  (:export :create-water
           :update-water))
(in-package :quaremain.models.stock.water)

(defun satisfies-constraints?
    (name
     description
     amount
     cost
     millilitres)
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
             
             (satisfies-length-constraint? millilitres 1 max-constraint)
             (satisfies-integer-constraint? millilitres))
      (error 'user-input-doesnt-satisfy-constraint-error))))

(defun create-water (water)
  (let ((name
         (get-key-value water :name))
        (description
         (get-key-value water :description))
        (amount
         (get-key-value water :amount))
        (cost-per-stock
         (get-key-value water :cost-per-stock))
        (millilitres-per-stock
         (get-key-value water :millilitres-per-stock)))
    
    (when (row-exist-by-name? :water name)
      (error 'row-with-same-name-already-exist-error
             :name name
             :table-name :water))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock
     millilitres-per-stock)
    
    (with-connection (db)
      (create-datum
          :water
        :name name
        :description description
        :amount amount
        :cost-per-stock cost-per-stock
        :millilitres-per-stock millilitres-per-stock))))

(defun update-water (water id)
  (let ((name
         (get-key-value water :name))
        (description
         (get-key-value water :description))
        (amount
         (get-key-value water :amount))
        (cost-per-stock
         (get-key-value water :cost-per-stock))
        (millilitres-per-stock
         (get-key-value water :millilitres-per-stock)))

    (satisfies-constraints?
     name
     description
     amount
     cost-per-stock
     millilitres-per-stock)
    
    (with-connection (db)
      (update-datum-by-id
          :water
          id
          name
          description
          amount
          cost-per-stock
        :millilitres-per-stock millilitres-per-stock))))
