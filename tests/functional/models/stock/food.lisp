(defpackage quaremain/tests/functional/models/stock/food
  (:use :cl
        :rove)
  (:import-from :quaremain.utilities.database
                :with-connection
                :db
                :migrate-tables
                :migrate-seeds
                :drop-tables)
  (:import-from :quaremain.models.stock.food
                :create-food
                :update-food)
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/models/stock/food)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))

(deftest create-food
    (create-food
     '((:name . "memizomuweuanwnean")
       (:description . "delicious instant bread")
       (:amount . 3)
       (:cost-per-package . 12.20d2)
       (:calories-per-package . 1200)))
  (with-connection (db)
    (let* ((result (database:get-datum-by-id :food 3))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   3))))))

(deftest update-food
    (update-food
     '((:name . "memozhahwhe8237hgwa")
       (:description . "")
       (:amount . 9293)
       (:cost-per-package . 90.00d2)
       (:calories-per-package . 9200))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :food 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   9293))))))
