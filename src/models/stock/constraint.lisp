(defpackage quaremain.models.stock.constraint
  (:use :cl)
  (:local-nicknames (#:regex #:cl-ppcre))
  (:export :satisfies-length-constraint?
           :satisfies-integer-constraint?
           :satisfies-decimal-constraint?
           :satisfies-string-constraint?))
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

(defun satisfies-integer-constraint?
    (input)
  (regex:scan "^[0-9]+$" input))

(defun satisfies-decimal-constraint?
    (input)
  (regex:scan "^[0-9]+\\.[0-9]+$" input))

(defun satisfies-string-constraint?
    (input)
  (regex:scan "^[a-zA-Z]+[a-zA-Z0-9\\.\\s!?\\-_\\$@&,]*$" input))
