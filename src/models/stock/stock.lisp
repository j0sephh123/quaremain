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
  
  (:import-from :datafly
                :execute)
  (:import-from :quaremain.utilities.database
                :db
                :with-connection
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

   (description
    :initform nil
    :initarg :description
    :reader get-description
    :writer set-description)

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

(defclass <water> (<stock>)
  ((millilitre-per-package
    :initform nil
    :initarg :millilitre-per-package
    :reader get-millilitre-per-package
    :writer set-millilitre-per-package)))

(defclass <medicine> (<stock>) nil)
(defclass <weapon> (<stock>) nil)

(defclass <calculator> ()
  ((stock-instance
    :initform nil
    :initarg :stock-instance
    :reader get-stock-instance)))

(defclass <stock-creator> ()
  ((stock-instance
    :initform nil
    :initarg :stock-instance
    :reader get-stock-instance)))

(defmethod new ((stock <stock>)))

(defmethod new ((food <food>))
  (with-connection (db)
    (execute
     (insert-datum-into-table :food
       :name (get-name food)
       :description (get-description food)
       :amount (get-amount food)
       :cost-per-package (get-cost-per-package food)
       :calories-per-package (get-calories-per-package food)))))

(defmethod new ((water <water>))
  (with-connection (db)
    (execute
     (insert-datum-into-table :water
       :name (get-name water)
       :description (get-description water)
       :amount (get-amount water)
       :cost-per-package (get-cost-per-package water)
       :millilitre-per-package (get-millilitre-per-package water)))))

(defmethod new ((medicine <medicine>))
  (with-connection (db)
    (execute
     (insert-datum-into-table :medicine
       :name (get-name medicine)
       :description (get-description medicine)
       :amount (get-amount medicine)
       :cost-per-package (get-cost-per-package medicine)))))

(defmethod new ((weapon <weapon>))
  (with-connection (db)
    (execute
     (insert-datum-into-table :weapon
       :name (get-name weapon)
       :description (get-description weapon)
       :amount (get-amount weapon)
       :cost-per-package (get-cost-per-package weapon)))))

(defun create-new-stock (stock-category name description
                         amount cost-per-package calories-per-package
                         millilitre-per-package)
  
  (cond ((string-equal stock-category "food")
         (new (make-instance '<food>
                             :name name
                             :description description
                             :amount amount
                             :cost-per-package cost-per-package
                             :calories-per-package calories-per-package)))

        ((string-equal stock-category "water")
         (new (make-instance '<water>
                             :name name
                             :description description
                             :amount amount
                             :cost-per-package cost-per-package
                             :millilitre-per-package millilitre-per-package)))

        ((string-equal stock-category "medicine")
         (new (make-instance '<medicine>
                             :name name
                             :description description
                             :amount amount
                             :cost-per-package cost-per-package)))

        ((string-equal stock-category "weapon")
         (new (make-instance '<weapon>
                             :name name
                             :description description
                             :amount amount
                             :cost-per-package cost-per-package)))))

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
         (with-connection (db)
           (execute
            (generate-update-datum
                (string-to-keyword stock-category)
                id
                name
                description
                amount
                cost-per-package
              :calories-per-package calories-per-package))))

        ((string-equal stock-category "water")
         (with-connection (db)
           (execute
            (generate-update-datum
                (string-to-keyword stock-category)
                id
                name
                description
                amount
                cost-per-package
              :millilitre-per-package millilitre-per-package))))))

(defun sum-stocks-from-table (table-name)
  (let ((table-data
         (get-all-datum-from-table table-name)))
    
    (sum-all-cost-per-package
     
     (cond ((eql table-name :food)
            (sum-all-calories-per-package table-data)
            table-data)
           ((eql table-name :water)
            (sum-all-millilitre-per-package table-data)
            table-data)))
    table-data))

(defun get-coerced-stock-by-category-and-id (category id)
  (let ((package
         (get-datum-from-table (string-to-keyword category) id)))
    (coerce-cost-per-package package)
    package))

(defun delete-stock-by-category-and-id (category id)
  (let ((table-name
         (string-to-keyword category)))
    (delete-datum-from-table table-name id)))
