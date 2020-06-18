;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.models.stock.stock
  (:documentation "Stock abstract model related procedures.")
  (:use :cl)
  (:import-from :quaremain.models.stock.food
                :create-food)
  (:import-from :quaremain.models.stock.water
                :create-water)
  (:import-from :quaremain.models.stock.medicine
                :create-medicine)
  (:import-from :quaremain.models.stock.weapon
                :create-weapon)
  
  (:import-from :quaremain.utilities.string
                :string->keyword
                :get-key-value)
  
  (:import-from :quaremain.utilities.database
                :db
                :with-connection
                :create-datum
                :get-all-datum
                :get-datum-by-id
                :update-datum-by-id
                :delete-datum-by-id
                :row-exist-by-name?
                :row-exist-by-id?)

  (:import-from :quaremain.utilities.exception
                :stock-missing-property-value-error
                :row-doesnt-exist-error
                :row-with-same-name-already-exist-error
                :total-required-survival-resources-is-too-low-error
                :all-stocks-empty-error)
  
  (:export :create-stock
           :update-stock-by-id
           :get-stocks-sum
           :get-coerced-stock-cost-by-id
           :delete-stock-by-id
           :get-total-survival-days))

(in-package :quaremain.models.stock.stock)

(defun create-stock (stock)
  (let ((stock-category
         (get-key-value stock :stock-category)))
    (cond ((string= stock-category "food")
           (create-food stock))
          ((string= stock-category "water")
           (create-water stock))
          ((string= stock-category "medicine")
           (create-medicine stock))
          ((string= stock-category "weapon")
           (create-weapon stock)))))

(defun sum-all-cost-per-stock (stocks)
  (dolist (stock stocks)
    (setf (getf stock :cost-per-package)
          (coerce (* (getf stock :amount)
                     (getf stock :cost-per-package))
                  'single-float)))
  stocks)

(defun sum-unique-property-value-by-amount (stock property-key)
  (setf (getf stock property-key)
        (* (getf stock :amount)
           (getf stock property-key)))
  stock)

(defun sum-all-calories-per-stock (stocks)
  (dolist (stock stocks)
    (sum-unique-property-value-by-amount stock :calories-per-package))
  stocks)

(defun sum-all-millilitre-per-stock (stocks)
  (dolist (stock stocks)
    (sum-unique-property-value-by-amount stock :millilitre-per-package))
  stocks)


(defun coerce-cost-per-stock (stock)
  (let ((cost-per-stock
         (getf stock :cost-per-package)))
    (setf (getf stock :cost-per-package)
          (coerce cost-per-stock 'single-float)))
  stock)


(defun update-stock-by-id (stock)
  (flet ((get-value (key)
           (cdr (assoc key stock))))
    (let ((stock-category
           (string->keyword
            (get-value :stock-category)))
          (id (get-value :id)))
      (with-connection (db)
        (unless (row-exist-by-id? stock-category id)
          (error 'row-doesnt-exist-error
                 :table-name stock-category
                 :id id))
        (cond
          ((eql stock-category :food)
           (update-datum-by-id
               :food
               id
               (get-value :name)
               (get-value :description)
               (get-value :amount)
               (get-value :cost-per-package)
             :calories-per-package (get-value :calories-per-package)))
          ((eql stock-category :water)
           (update-datum-by-id
               :water
               id
               (get-value :name)
               (get-value :description)
               (get-value :amount)
               (get-value :cost-per-package)
             :millilitre-per-package (get-value :millilitre-per-package)))
          (t
           (update-datum-by-id
               stock-category
               id
               (get-value :name)
               (get-value :description)
               (get-value :amount)
               (get-value :cost-per-package))))))))

(defun get-stocks-sum (table-name)
  (let ((stocks
         (with-connection (db)
           (get-all-datum table-name))))
    
    (sum-all-cost-per-stock
     (cond ((eql table-name :food)
            (sum-all-calories-per-stock stocks)
            stocks)           
           ((eql table-name :water)
            (sum-all-millilitre-per-stock stocks)
            stocks)
           (t
            stocks)))
    stocks))

(defun get-coerced-stock-cost-by-id (stock-category id)
  (with-connection (db)
    (let ((table-name
           (string->keyword stock-category)))      
      (unless (row-exist-by-id? table-name id)
        (error 'row-doesnt-exist-error
               :table-name stock-category
               :id id))

      (let ((stock
             (get-datum-by-id table-name id)))
        (coerce-cost-per-stock stock)
        stock))))

(defun delete-stock-by-id (stock-category id)
  (with-connection (db)
    (let ((table-name
           (string->keyword stock-category)))
      (unless (row-exist-by-id? table-name id)
        (error 'row-doesnt-exist-error
               :table-name table-name
               :id id))
      (delete-datum-by-id table-name id))))

(defun get-total-unique-property-stock-value-sum (unique-property
                                                  stocks)
  (with-connection (db)
    (reduce #'+
            (mapcar #'(lambda (stock)
                        (getf stock  unique-property))
                    stocks))))

(defun get-total-food-calories ()
  (with-connection (db)
    (get-total-unique-property-stock-value-sum
     :calories-per-package
     (sum-all-calories-per-stock
      (get-all-datum :food)))))

(defun get-total-water-millilitre ()
  (with-connection (db)
    (get-total-unique-property-stock-value-sum
     :millilitre-per-package
     (sum-all-millilitre-per-stock
      (get-all-datum :water)))))

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

(defun get-all-stocks ()
  (with-connection (db)
    (let ((food-stocks
           (get-all-datum :food))
          (water-stocks
           (get-all-datum :water))
          (medicine-stocks
           (get-all-datum :medicine))
          (weapon-stocks
           (get-all-datum :weapon)))
      (when (and
             (null food-stocks)
             (null water-stocks)
             (null medicine-stocks)
             (null weapon-stocks))
        (error 'all-stocks-empty-error))
      (list
       :food food-stocks
       :water water-stocks
       :medicine medicine-stocks
       :weapon weapon-stocks))))
