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
                :satisfies-length-constraint?)
  (:export :create-water
           :update-water))
(in-package :quaremain.models.stock.water)

(defun create-water (water)
  (let ((name
         (get-key-value water :name))
        (description
         (get-key-value water :description))
        (amount
         (get-key-value water :amount))
        (cost-per-package
         (get-key-value water :cost-per-package))
        (millilitre-per-package
         (get-key-value water :millilitre-per-package)))
    
    (when (row-exist-by-name? :water name)
      (error 'row-with-same-name-already-exist-error
             :name name
             :table-name :water))

    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-package 1 9999999999999)
             (satisfies-length-constraint? millilitre-per-package 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))
    
    (with-connection (db)
      (create-datum
       :water
       :name name
       :description description
       :amount amount
       :cost-per-package cost-per-package
       :millilitre-per-package millilitre-per-package))))

(defun update-water (water id)
  (let ((name
         (get-key-value water :name))
        (description
         (get-key-value water :description))
        (amount
         (get-key-value water :amount))
        (cost-per-package
         (get-key-value water :cost-per-package))
        (millilitre-per-package
         (get-key-value water :millilitre-per-package)))

    (unless (= (length description) 0)
      (unless (satisfies-length-constraint? description 20 1500)
        (error 'user-input-doesnt-satisfy-constraint-error)))

    (unless (and
             (satisfies-length-constraint? name 5 250)
             (satisfies-length-constraint? amount 1 999999999)
             (satisfies-length-constraint? cost-per-package 1 9999999999999)
             (satisfies-length-constraint? millilitre-per-package 1 9999999999999))
      (error 'user-input-doesnt-satisfy-constraint-error))
    
    (with-connection (db)
      (update-datum-by-id
       :water
       id
       name
       description
       amount
       cost-per-package
       :millilitre-per-package millilitre-per-package))))
