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
     '((:name . "fjki7hhawhehawhe")
       (:description . "")
       (:amount . "9")
       (:cost-per-stock . "3.20")
       (:millilitres-per-stock . "1200")))
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
     '((:name . "fireppe882u3u23283")
       (:description . "mustoz82838283283823")
       (:amount . "14")
       (:cost-per-stock . "1.00")
       (:millilitres-per-stock . "1900"))
     1)
  (with-connection (db)
    (let* ((result (database::get-datum-by-id :water 1))
           (amount
            (getf result :amount)))
      (testing ""
               (ok
                (= amount
                   14))))))
