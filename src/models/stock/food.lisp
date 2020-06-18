(defpackage quaremain.models.stock.food
  (:use :cl)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum)
  (:export :create-food))
(in-package :quaremain.models.stock.food)

(defun create-food (food)
  (flet ((get-value (key)
           (cdr (assoc key food))))
    (with-connection (db)
      (create-datum
       :food
       :name (get-value :name)
       :description (get-value :description)
       :amount (get-value :amount)
       :cost-per-package (get-value :cost-per-package)
       :calories-per-package (get-value :calories-per-package)))))
