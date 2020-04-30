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
(defpackage quaremain.web
  (:documentation "Server routing handler.")
  (:use :cl
        :caveman2
        :quaremain.utilities.config
        :quaremain.view)
  (:import-from :quaremain.utilities.database
                :db
                :with-connection
                :with-connection-execute
                :insert-datum-into-table
                :get-all-datum-from-table
                :get-datum-by-id
                :generate-update-datum-by-id
                :delete-datum-by-id)
  (:import-from :quaremain.utilities.string
                :string-to-keyword)
  (:import-from :quaremain.models.stock.stock
                :create-new-stock)
  (:export :*web*))
(in-package :quaremain.web)

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>)
  "Quaremain's system wide application instance.")
(clear-routing-rules *web*)

(defparameter *session* (make-hash-table))

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

(defun do-update-datum-by-id (stock-category
                              id
                              name
                              description
                              amount
                              cost-per-package
                              calories-per-package)
  (if (string-equal stock-category "food")
      (with-connection-execute
        (generate-update-datum-by-id
            (string-to-keyword stock-category)
            id
            :name name
            :description description
          :amount amount
          :cost-per-package cost-per-package
          :calories-per-package calories-per-package))

      (with-connection-execute
        (generate-update-datum-by-id
            (string-to-keyword stock-category)
            id
            :name name
            :description description
          :amount amount
          :cost-per-package cost-per-package))))

(defun sum-data-from-table (table-name)
  (let ((table-data
         (get-all-datum-from-table table-name)))
    
    (sum-all-cost-per-package
     (if (eql table-name :food)
         (sum-all-calories-per-package table-data)
         table-data))
    table-data))

(defroute "/" ()
  (render #p"app/list.html"
          `(:data ,(sum-data-from-table :food)
                  :list-type "food")))

(defroute "/app/list/food" ()
  (render #p"app/list.html"
          `(:data ,(sum-data-from-table :food)
                  :list-type "food")))

(defroute "/app/list/water" ()
  (render #p"app/list.html"
          `(:data ,(sum-data-from-table :water)
                  :list-type "water")))

(defroute "/app/list/medicine" ()
  (render #p"app/list.html"
          `(:data ,(sum-data-from-table :medicine)
                  :list-type "medicine")))

(defroute "/app/list/weapon" ()
  (render #p"app/list.html"
          `(:data ,(sum-data-from-table :weapon)
                  :list-type "weapon")))

(defroute "/about" ()
  (render #p"about.html"))

(defroute "/app/create-form" ()
  (render #p"app/create-form.html"))

(defroute ("/app/create" :method :POST) (&key
                                         |stock-category|
                                         |name|
                                         |description|
                                         |amount|
                                         |cost-per-package|
                                         |calories-per-package|)

  (create-new-stock |stock-category|
                    |name|
                    |description|
                    |amount|
                    |cost-per-package|
                    |calories-per-package|)
  (redirect
   (format nil "/app/list/~A" |stock-category|)))

(defroute "/app/update-form/:id" (&key id
                                       |stock-category|)
  (setf (gethash 'datum-id *session*) id)
  (setf (gethash 'datum-stock-category *session*) |stock-category|)
  (render #p"app/update-form.html"
          (let ((coerced-datum
                 (coerce-cost-per-package
                  (get-datum-by-id
                   (string-to-keyword |stock-category|)
                   id))))
            (list :datum coerced-datum
                  :list-type |stock-category|))))

(defroute ("/app/update" :method :POST) (&key |name|
                                              |description|
                                              |amount|
                                              |cost-per-package|
                                              |calories-per-package|)

  (let* ((id (gethash 'datum-id *session*))
         (stock-category
          (gethash 'datum-stock-category *session*)))
    (do-update-datum-by-id stock-category
      id
      |name|
      |description|
      |amount|
      |cost-per-package|
      |calories-per-package|)
    (redirect
     (format nil "/app/list/~A" stock-category))))

(defroute ("/app/delete/:id" :method :GET) (&key id
                                                 |stock-category|)
  (let ((table-name
         (string-to-keyword |stock-category|)))
    (handler-case
        (delete-datum-from-table table-name id)
      
      (DBI.ERROR:DBI-PROGRAMMING-ERROR (exception)
        (log:error
         "~A"
         exception))))
  (redirect (format nil "/app/list/~A"
                    |stock-category|)))

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
