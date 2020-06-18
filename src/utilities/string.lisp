;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.utilities.string
  (:documentation "String utilities.")
  (:use :cl)
  (:export :string->keyword
           :get-key-value))
(in-package :quaremain.utilities.string)

(defun string->keyword (string)
  (read-from-string
   (format nil ":~A" string)))

(defun get-key-value (alist keyword)
  (cdr (assoc keyword alist)))
