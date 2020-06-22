(defpackage quaremain/tests/functional/models/stock/medicine
  (:use :cl
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :migrate-tables
                :migrate-seeds
                :drop-tables)
  (:import-from :quaremain.models.stock.medicine
                :create-medicine
                :update-medicine)
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/models/stock/medicine)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

(deftest create-medicine
    (create-medicine
     '((:name . "Paracetamol")
       (:description . "")
       (:amount . 90)
       (:cost-per-package . 12.20d2)))
  (with-connection (db)
    (let* ((result (database:get-datum-by-id :medicine 3))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   90))))))

(deftest update-medicine
    (update-medicine
     '((:name . "aspirin")
       (:description . "for headache")
       (:amount . 27)
       (:cost-per-package . 3.00d2))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :medicine 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   27))))))
