;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(defpackage quaremain.models.stock.survival
  (:use :cl)
  (:import-from :quaremain.models.stock.stock
                :get-total-food-calories
                :get-total-water-millilitre)
  (:import-from :quaremain.utilities.exception
                :total-required-survival-resources-is-too-low-error)
  (:export :survival-days-type))
(in-package :quaremain.models.stock.survival)

(defun calculate-total-survival-days (calories-sum
                                      millilitre-sum)
  (let ((minimal-calories-per-day 1500)
        (minimal-millilitre-per-day 2300))
    (when
        ;; Less than minimum
        (or (<= calories-sum minimal-calories-per-day)
            (<= millilitre-sum minimal-millilitre-per-day))
      (error 'total-required-survival-resources-is-too-low-error))

    
    ;; Calculate water intake first due to how
    ;; important it is to sustain a life of
    ;; a human being. A human being can survive
    ;; far longer with enough water even without
    ;; food compared to vice-versa (unless the
    ;; food contains enough water)
    (if (< millilitre-sum calories-sum)
        (floor
         millilitre-sum
         minimal-millilitre-per-day)
        (floor
         calories-sum
         minimal-calories-per-day))))

(defun get-total-survival-days ()
  (calculate-total-survival-days
   (get-total-food-calories)
   (get-total-water-millilitre)))

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
