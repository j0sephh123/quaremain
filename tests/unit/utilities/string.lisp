(defpackage quaremain/tests/unit/utilities/string
  (:use :cl
        :quaremain
        :rove)
  (:import-from :quaremain.utilities.string
                :string->keyword
                :get-key-value))
(in-package :quaremain/tests/unit/utilities/string)

(deftest quaremain.utilities.string
    (testing ""
             (ok
              (eql
               (string->keyword "fruitz")
               :fruitz))
             (ok
              (string=
               (get-key-value '((:fire . "ash")) :fire)
               "ash"))))
