(defpackage quaremain/tests/main
  (:use :cl
        :quaremain
        :rove))
(in-package :quaremain/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :quaremain)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
           (ok (= 1 1))))
