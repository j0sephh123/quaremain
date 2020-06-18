(defpackage quaremain/tests/unit/web
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.web
                :get-total-survival-days-alert-type))
(in-package :quaremain/tests/unit/web)

(deftest web-get-total-survival-days-alert-type
    (let ((result1 (get-total-survival-days-alert-type 50))
          (result2 (get-total-survival-days-alert-type 30))
          (result3 (get-total-survival-days-alert-type 24))
          (result4 (get-total-survival-days-alert-type 7))
          (result5 (get-total-survival-days-alert-type 6)))      
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

