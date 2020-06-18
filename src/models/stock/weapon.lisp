(defpackage quaremain.models.stock.weapon
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum)
  (:export :create-weapon))
(in-package :quaremain.models.stock.weapon)

(defun create-weapon (weapon)
  (let ((name
         (get-key-value weapon :name))
        (description
         (get-key-value weapon :description))
        (amount
         (get-key-value weapon :amount))
        (cost-per-package
         (get-key-value weapon :cost-per-package)))
    (with-connection (db)
      (create-datum
       :weapon
       :name name
       :description description
       :amount amount
       :cost-per-package cost-per-package))))
