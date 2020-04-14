(defpackage quaremain/tests/main
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :quaremain)' in your Lisp.

(deftest unit-tests
  (testing "(sum-model '((:cost-per-package 2.2 :amount 3))) should equal to ((:cost-per-package 6.6 :amount 3)"
           (let* ((raw-result
                   (quaremain.web::sum-model '((:cost-per-package 2.2 :amount 3))))
                  (total-cost (getf (car raw-result) :cost-per-package)))
             (ok
              (= total-cost 6.6000004)))))

