;;;; Quaremain - A software to manage resources for emergency times.
;;;; Copyright (C) 2020  Momozor <skelic3@gmail.com, momozor4@gmail.com>

;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-user)
(defpackage quaremain.models.stock.stock
  (:documentation "Stock abstract model related procedures.")
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :string-to-keyword)
  
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
                :row-with-same-name-already-exist-error)
  
  (:export :create-stock
           :update-stock-by-id
           :get-stocks-sum
           :get-coerced-stock-cost-by-id
           :delete-stock-by-id))

(in-package :quaremain.models.stock.stock)

(defun create-stock (&key
                       stock-category
                       name
                       description
                       amount
                       cost-per-package
                       calories-per-package
                       millilitre-per-package)
  
  (with-connection (db)
    
    (cond ((string-equal stock-category "food")
           (create-datum
            :food
            :name name
            :description description
            :amount amount
            :cost-per-package cost-per-package
            :calories-per-package calories-per-package))

          ((string-equal stock-category "water")
           (create-datum
            :water
            :name name
            :description description
            :amount amount
            :cost-per-package cost-per-package
            :millilitre-per-package millilitre-per-package))

          ((string-equal stock-category "medicine")
           (create-datum
            :medicine
            :name name
            :description description
            :amount amount
            :cost-per-package cost-per-package))

          ((string-equal stock-category "weapon")
           (create-datum
            :weapon
            :name name
            :description description
            :amount amount
            :cost-per-package cost-per-package)))))

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


(defun update-stock-by-id (&key
                             stock-category
                             id
                             name
                             description
                             amount
                             cost-per-package
                             calories-per-package
                             millilitre-per-package)

  (let ((table-name
         (string-to-keyword stock-category)))
    
    (cond
      ((eql table-name :food)
       
       (unless (row-exist-by-id? table-name id)
         (error 'row-doesnt-exist-error
                :table-name table-name
                :id id))
       
       (with-connection (db)
         (update-datum-by-id
             table-name
             id
             name
             description
             amount
             cost-per-package
           :calories-per-package calories-per-package)))

      ((eql table-name :food)

       (unless (row-exist-by-id? table-name id)
         (error 'row-doesnt-exist-error
                :table-name table-name
                :id id))
       
       (with-connection (db)
         (update-datum-by-id
             table-name
             id
             name
             description
             amount
             cost-per-package
           :millilitre-per-package millilitre-per-package)))

      (t

       (unless (row-exist-by-id? table-name id)
         (error 'row-doesnt-exist-error
                :table-name table-name
                :id id))
       
       (with-connection (db)
         (update-datum-by-id
             table-name
             id
             name
             description
             amount
             cost-per-package))))))

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
  (let ((table-name
         (string-to-keyword stock-category)))
    
    (unless (row-exist-by-id? table-name id)
      (error 'row-doesnt-exist-error
             :table-name stock-category
             :id id))

    (let ((stock
           (with-connection (db)
             
             (get-datum-by-id table-name id))))
      (coerce-cost-per-stock stock)
      stock)))

(defun delete-stock-by-id (stock-category id)
  (with-connection (db)
    
    (let ((table-name
           (string-to-keyword stock-category)))
      
      (unless (row-exist-by-id? table-name id)
        (error 'row-doesnt-exist-error
               :table-name table-name
               :id id))
      
      (delete-datum-by-id table-name id))))
