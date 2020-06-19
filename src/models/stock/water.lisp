(defpackage quaremain.models.stock.water
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum
                :update-datum-by-id)
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
    (update-datum-by-id
        :water
        id
        name
        description
        amount
        cost-per-package
      :millilitre-per-package millilitre-per-package)))
