(defpackage quaremain.models.stock.constraint
  (:use :cl)
  (:export :satisfies-length-constraint?))
(in-package :quaremain.models.stock.constraint)

(defmethod satisfies-length-constraint?
    ((input string)
     min-constraint
     max-constraint)
  (and
   (>= (length input) min-constraint)
   (<= (length input) max-constraint)))

(defmethod satisfies-length-constraint?
    ((input number)
     min-constraint
     max-constraint)
  (and
   (>= input min-constraint)
   (<= input max-constraint)))
