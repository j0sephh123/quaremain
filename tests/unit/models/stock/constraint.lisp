(defpackage quaremain/tests/unit/models/stock/constraint
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.models.stock.constraint
                :satisfies-length-constraint?
                :satisfies-integer-constraint?
                :satisfies-decimal-constraint?
                :satisfies-string-constraint?))
(in-package :quaremain/tests/unit/models/stock/constraint)

(deftest length-constraints
  (let ((max-constraint 99999999999))
  (testing "double-float"
           (ok (satisfies-length-constraint?
                   1727.04d0
                   1
                   max-constraint)))
  (testing "integer"
           (ok
            (satisfies-length-constraint?
                    97723
                    1
                    max-constraint)))
  (testing "string-1"
           (ok
            (satisfies-length-constraint?
                    "meto om mato!"
                    1
                    max-constraint)))))

;; TODO: Add more test cases
(deftest user-characters-input-type-constraints
  (testing "integer"
           (ok (satisfies-integer-constraint?
                "82382838238"))
           (ng (satisfies-integer-constraint?
                "2883.9293")))
  (testing "decimal"
           (ok (satisfies-decimal-constraint?
                "8238283.92"))
           (ng (satisfies-decimal-constraint?
                "8283823")))
  (testing "string"
           (ok (satisfies-string-constraint?
                "ometa -!$"))
           (ok (satisfies-string-constraint?
                "wuw uw $"))
           (ok (satisfies-string-constraint?
                "hwuuwey83!&@$."))
           (ok (satisfies-string-constraint?
                "hwaheh"))
           (ok (satisfies-string-constraint?
                "w"))
           (ok (satisfies-string-constraint?
                "uuw77 273HWWE,uu 238a!"))
           (ng (satisfies-string-constraint?
                "uuwu9 237@ ! #hahwH&"))
           (ng (satisfies-string-constraint?
                " w"))
           (ng (satisfies-string-constraint?
                "**#$&&hhahwe92"))
           (ng (satisfies-string-constraint?
                "7273hwhe)!#*"))))
