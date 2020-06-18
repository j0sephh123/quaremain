(defpackage quaremain.models.stock.medicine
  (:use :cl)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum)
  (:export :create-medicine))
(in-package :quaremain.models.stock.medicine)

(defun create-medicine (medicine)
  (flet ((get-value (key)
           (cdr (assoc key medicine))))
    (with-connection (db)
      (create-datum
       :medicine
       :name (get-value :name)
       :description (get-value :description)
       :amount (get-value :amount)
       :cost-per-package (get-value :cost-per-package)))))
