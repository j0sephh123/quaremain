;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(defpackage quaremain.models.stock.survival
  (:use :cl)
  (:export :survival-days-type))
(in-package :quaremain.models.stock.survival)

(defun survival-days-type (total-survival-days)
  (let ((week 7)
        (month 30))
    (cond
      ((<= total-survival-days week)
       "warning")
      ((and (> total-survival-days week)
            (<= total-survival-days month))
       "info")
      ((> total-survival-days month)
       "success"))))
