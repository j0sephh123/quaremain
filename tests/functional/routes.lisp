(defpackage quaremain/tests/functional/routes
  (:use :cl
        :quaremain
        :rove)
  (:local-nicknames (#:routes #:quaremain.web))
  (:local-nicknames (#:database #:quaremain.utilities.database)))
(in-package :quaremain/tests/functional/routes)

(setup
  (database::migrate-tables)
  (database::migrate-seeds))

(teardown
  (database::drop-tables))

(deftest tests
  (testing ""
    (ok (= 1 1))))
