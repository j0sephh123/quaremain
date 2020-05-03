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
           :sum-all-cost-per-package
           :sum-all-calories-per-package
           :coerce-cost-per-package
           :update-stock-by-category-and-id
           :sum-stocks-from-table
           :get-coerced-stock-by-category-and-id
           :delete-stock-by-category-and-id))
(in-package :quaremain.models.stock.stock)


(defun create-new-stock (stock-category name description
                         amount cost-per-package calories-per-package)
  (if (string-equal stock-category "food")
      (progn
        (when (or (string-equal calories-per-package "")
                  (null calories-per-package))
          (error 'stock-missing-property-value-error
                 :message "Calories per package is empty."))
        (insert-datum-into-table (string-to-keyword stock-category)
          :name name
          :description description
          :amount amount
          :cost-per-package cost-per-package
          :calories-per-package calories-per-package))

      (insert-datum-into-table (string-to-keyword stock-category)
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
                                        calories-per-package)
  (if (string-equal stock-category "food")
      (with-connection-execute
        (generate-update-datum
         (string-to-keyword stock-category)
         id
         name
         description
         amount
         cost-per-package
         :calories-per-package calories-per-package))

      (with-connection-execute
        (generate-update-datum
         (string-to-keyword stock-category)
         id
         name
         description
         amount
         cost-per-package))))

(defun sum-stocks-from-table (table-name)
  (let ((table-data
         (get-all-datum-from-table table-name)))
    
    (sum-all-cost-per-package
     (if (eql table-name :food)
         (sum-all-calories-per-package table-data)
         table-data))
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
