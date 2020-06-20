(defpackage quaremain/tests/unit/models/stock/survival
  (:use :cl :rove)
  (:import-from :quaremain.models.stock.survival
                :survival-days-type))
(in-package :quaremain/tests/unit/models/stock/survival)

(deftest survival-days-type
  (let ((result1 (survival-days-type 50))
        (result2 (survival-days-type 30))
        (result3 (survival-days-type 24))
        (result4 (survival-days-type 7))
        (result5 (survival-days-type 6)))      
    (testing ""
             (ok
              (string-equal result1
                            "success")))
    (testing ""
             (ok
              (string-equal result2
                            "info")))
    (testing ""
             (ok
              (string-equal result3
                            "info")))
    (testing ""
             (ok
              (string-equal result4
                            "warning")))
    (testing ""
             (ok
              (string-equal result5
                            "warning")))))


