(defpackage quaremain.models.stock.medicine
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum)
  (:export :create-medicine))
(in-package :quaremain.models.stock.medicine)

(defun create-medicine (medicine)
  (let ((name
         (get-key-value medicine :name))
        (description
         (get-key-value medicine :description))
        (amount
         (get-key-value medicine :amount))
        (cost-per-package
         (get-key-value medicine :cost-per-package)))
    (with-connection (db)
      (create-datum
       :medicine 
       :name name
       :description description
       :amount amount
       :cost-per-package cost-per-package))))
