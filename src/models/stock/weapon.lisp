(defpackage quaremain.models.stock.weapon
  (:use :cl)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db)
  (:export :create-weapon))
(in-package :quaremain.models.stock.weapon)

(defun create-weapon (weapon)
  (flet ((get-value (key)
           (cdr (assoc key weapon))))
    (with-connection (db)
      (create-datum
       :food
       :name (get-value :name)
       :description (get-value :description)
       :amount (get-value :amount)
       :cost-per-package (get-value :cost-per-package)))))
