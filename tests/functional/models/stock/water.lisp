(defpackage quaremain/tests/functional/models/stock/water
  (:use :cl
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :migrate-tables
                :migrate-seeds
                :drop-tables)
  (:import-from :quaremain.models.stock.water
                :create-water
                :update-water)
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/models/stock/water)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

(deftest create-water
    (create-water
     '((:name . "fjki")
       (:description . "")
       (:amount . 9)
       (:cost-per-package . 3.20d2)
       (:millilitre-per-package . 1200)))
  (with-connection (db)
    (let* ((result (database:get-datum-by-id :water 3))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   9))))))

(deftest update-water
    (update-water
     '((:name . "fireppe")
       (:description . "mustoz")
       (:amount . 14)
       (:cost-per-package . 1.00d2)
       (:millilitre-per-package . 1900))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :water 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   14))))))
