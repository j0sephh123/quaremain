(defpackage quaremain/tests/functional/models/stock/weapon
  (:use :cl
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :migrate-tables
                :migrate-seeds
                :drop-tables)
  (:import-from :quaremain.models.stock.weapon
                :create-weapon
                :update-weapon)
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/models/stock/weapon)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

(deftest create-weapon
    (create-weapon
     '((:name . "AR-15")
       (:description . "Powerful automatic rifle")
       (:amount . 1)
       (:cost-per-package . 3200.20d2)))
  (with-connection (db)
    (let* ((result (database:get-datum-by-id :weapon 3))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   1))))))

(deftest update-weapon
    (update-weapon
     '((:name . "FAL")
       (:description . "Piercing semi-automatic rifle")
       (:amount . 3)
       (:cost-per-package . 2600.00d2))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :weapon 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   3))))))
