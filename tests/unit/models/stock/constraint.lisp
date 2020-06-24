(defpackage quaremain/tests/unit/models/stock/constraint
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.models.stock.constraint
                :satisfies-length-constraint?))
(in-package :quaremain/tests/unit/models/stock/constraint)

(deftest length-constraints
  (testing "double-float"
           (ok
            (not
             (null (satisfies-length-constraint?
                   1727.04d0
                   1
                   99999999999)))))
  (testing "integer"
           (ok
            (not
             (null (satisfies-length-constraint?
                    97723
                    1
                    99999999999)))))
  (testing "string-1"
           (ok
            (not
             (null (satisfies-length-constraint?
                    "meto om mato!"
                    1
                    99999999999))))))
