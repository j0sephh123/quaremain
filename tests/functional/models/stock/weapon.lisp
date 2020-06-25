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
       (:description . "Powerful automatic rifle. medium calibre, suitable for long range combat")
       (:amount . "1")
       (:cost-per-stock . "3200.20")))
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
     '((:name . "FAL1092-NATO")
       (:description . "")
       (:amount . "3")
       (:cost-per-stock . "2600.00"))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :weapon 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   3))))))
