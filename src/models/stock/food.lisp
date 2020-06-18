(defpackage quaremain.models.stock.food
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :create-datum)
  (:export :create-food))
(in-package :quaremain.models.stock.food)

(defun create-food (food)
  (let ((name
         (get-key-value food :name))
        (description
         (get-key-value food :description))
        (amount
         (get-key-value food :amount))
        (cost-per-package
         (get-key-value food :cost-per-package))
        (calories-per-package
         (get-key-value food :calories-per-package)))
    (with-connection (db)
      (create-datum
       :food
       :name name
       :description description
       :amount amount
       :cost-per-package cost-per-package
       :calories-per-package calories-per-package))))
