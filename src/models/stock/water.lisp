(defpackage quaremain.models.stock.water
  (:use :cl)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db)
  (:export :create-water))
(in-package :quaremain.models.stock.water)

(defun create-water (water)
  (flet ((get-value (key)
           (cdr (assoc key water))))
    (with-connection (db)
      (create-datum
       :food
       :name (get-value :name)
       :description (get-value :description)
       :amount (get-value :amount)
       :cost-per-package (get-value :cost-per-package)
       :millilitre-per-package (get-value :millilitre-per-package)))))
