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
                :create-food))
(in-package :quaremain/tests/functional/models/stock/food)

(setup
 (migrate-tables)
 (migrate-seeds))

(teardown
 (drop-tables))
