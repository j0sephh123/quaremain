(defpackage quaremain/tests/unit/web
  (:use :cl
        :rove)
  (:import-from :quaremain.web
                :get-status-code))
(in-package :quaremain/tests/unit/web)

(deftest get-status-code
  (testing "success"
           (ok
            (= (get-status-code :success)
               200)))
  (testing "not found"
           (ok
            (= (get-status-code :not-found)
               404))))
