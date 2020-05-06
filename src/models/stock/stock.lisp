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
                :with-connection-execute
                :insert-datum-into-table
                :get-all-datum-from-table
                :get-datum-from-table
                :generate-update-datum
                :delete-datum-from-table)

  (:import-from :quaremain.utilities.exception
                :stock-missing-property-value-error)
  
  (:export :create-new-stock
           :update-stock-by-category-and-id
           :sum-stocks-from-table
           :get-coerced-stock-by-category-and-id
           :delete-stock-by-category-and-id))
(in-package :quaremain.models.stock.stock)

(defclass <stock> ()
  ((id
    :initform nil
    :initarg :id
    :reader get-id
    :writer set-id)

   (name
    :initform nil
    :initarg :name
    :reader get-name
    :writer set-name)

   (amount
    :initform nil
    :initarg :amount
    :reader get-amount
    :writer set-amount)

   (cost-per-package
    :initform nil
    :initarg :cost-per-package
    :reader get-cost-per-package
    :writer set-cost-per-package)))

(defclass <food> (<stock>)
  ((calories-per-package
    :initform nil
    :initarg :calories-per-package
    :reader get-calories-per-package
    :writer set-calories-per-package)))
(defclass <water> (<stock>) ())
(defclass <medicine> (<stock>) ())
(defclass <weapon> (<stock>) ())

(defclass <calculator> ()
  ((stock-instance
    :initform nil
    :initarg :stock-instance
    :accessor stock-instance)))

(defmethod property-value-empty? ((food <food>))
  (or (null (get-calories-per-package food))
      (string-equal (get-calories-per-package food)
                    "")))

(defun item-value-is-empty? (item)
  (or (null item)
      (string-equal item "")))

(defun create-food-stock (&key name description amount
                            cost-per-package calories-per-package)
  (when (item-value-is-empty? calories-per-package)
    (error 'stock-missing-property-value-error
           :message "Calories per package is empty."))
  (insert-datum-into-table :food
    :name name
    :description description
    :amount amount
    :cost-per-package cost-per-package
    :calories-per-package calories-per-package))

(defun create-water-stock (&key name description amount
                             cost-per-package millilitre-per-package)


  (when (item-value-is-empty? millilitre-per-package)
    (error 'stock-missing-property-value-error
           :message "Millilitre-per-package is missing."))
  (insert-datum-into-table :water
    :name name
    :description description
    :amount amount
    :cost-per-package cost-per-package
    :millilitre-per-package millilitre-per-package))

(defun create-new-stock (stock-category name description
                         amount cost-per-package calories-per-package
                         millilitre-per-package)
  
  (cond ((string-equal stock-category "food")
         (create-food-stock :name name :description description
                            :amount amount :cost-per-package cost-per-package
                            :calories-per-package calories-per-package))

        ((string-equal stock-category "water")
         (create-water-stock :name name :description description
                             :amount amount :cost-per-package cost-per-package
                             :millilitre-per-package millilitre-per-package))

        (t
         (insert-datum-into-table (string-to-keyword stock-category))
         :name name
         :description description
         :amount amount
         :cost-per-package cost-per-package)))

(defun sum-all-cost-per-package (packages)
  (loop for package in packages
     do (setf (getf package :cost-per-package)
              (coerce (* (getf package :amount)
                         (getf package :cost-per-package))
                      'single-float)))
  packages)

(defun sum-all-calories-per-package (packages)
  (loop for package in packages
     do (setf (getf package :calories-per-package)
              (* (getf package :amount)
                 (getf package :calories-per-package))))
  packages)

(defun sum-all-millilitre-per-package (packages)
  (loop for package in packages
     do (setf (getf package :millilitre-per-package)
              (* (getf package :amount)
                 (getf package :millilitre-per-package))))
  packages)


(defun coerce-cost-per-package (package)
  (let ((cost-per-package (getf package :cost-per-package)))
    (setf (getf package :cost-per-package)
          (coerce cost-per-package 'single-float)))
  package)


(defun update-stock-by-category-and-id (stock-category
                                        id
                                        name
                                        description
                                        amount
                                        cost-per-package
                                        calories-per-package
                                        millilitre-per-package)
  (cond ((string-equal stock-category "food")
         (with-connection-execute
           (generate-update-datum
               (string-to-keyword stock-category)
               id
               name
               description
               amount
               cost-per-package
             :calories-per-package calories-per-package)))

        ((string-equal stock-category "water")
         (with-connection-execute
           (generate-update-datum
               (string-to-keyword stock-category)
               id
               name
               description
               amount
               cost-per-package
             :millilitre-per-package millilitre-per-package)))))

(defun sum-stocks-from-table (table-name)
  (let ((table-data
         (get-all-datum-from-table table-name)))
    
    (sum-all-cost-per-package
     (cond ((eql table-name :food)
            (sum-all-calories-per-package table-data)
            table-data)

           ((eql table-name :water)
            (sum-all-millilitre-per-package table-data)
            table-data)))))

(defun get-coerced-stock-by-category-and-id (category id)
  (let ((package
         (get-datum-from-table (string-to-keyword category) id)))
    (coerce-cost-per-package package)
    package))

(defun delete-stock-by-category-and-id (category id)
  (let ((table-name
         (string-to-keyword category)))
    (delete-datum-from-table table-name id)))
