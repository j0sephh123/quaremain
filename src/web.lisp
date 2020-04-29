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
        :quaremain.config
        :quaremain.view)
  (:import-from :quaremain.db
                :db
                :with-connection
                :with-connection-execute
                :insert-datum-into-table
                :get-all-datum-from-table)
  (:export :*web*))
(in-package :quaremain.web)

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>)
  "Quaremain's system wide application instance.")
(clear-routing-rules *web*)

(defparameter *session* (make-hash-table))

(defun sum-all-cost-per-package (plists)
  (loop for plist in plists
     do (setf (getf plist :cost-per-package)
              (coerce (* (getf plist :amount)
                         (getf plist :cost-per-package))
                      'single-float)))
  plists)

(defun sum-all-calories-per-package (plists)
  (loop for plist in plists
     do (setf (getf plist :calories-per-package)
              (* (getf plist :amount)
                 (getf plist :calories-per-package))))
  plists)

(defun coerce-cost-per-package (plist)
  (let ((cost-per-package (getf plist :cost-per-package)))
    (setf (getf plist :cost-per-package)
          (coerce cost-per-package 'single-float)))
  plist)

(defun get-datum-by-id (table-name id)
  (with-connection (db)
    (datafly:retrieve-one
     (sxql:select :* (sxql:from table-name)
                  (sxql:where (:= :id id))))))

(defmacro generate-update-datum-by-id (table-name id
                                       name
                                       description
                                       amount
                                       cost-per-package
                                       &body sxql-column-specifier-forms)
  `(sxql:update ,table-name
     (sxql:set= :name ,name
                :description ,description
                :amount ,amount
                :cost-per-package ,cost-per-package
                ,@sxql-column-specifier-forms)
     (sxql:where (:= :id ,id))))

(defun delete-datum-from-table (table-name id)
  (with-connection-execute
    (sxql:delete-from table-name
      (sxql:where (:= :id id)))))

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

(defun string-to-keyword (string)
  (read-from-string
   (format nil ":~A" string)))

(defun create-new-stock (stock-category name description
                         amount cost-per-package calories-per-package)
  (if (string-equal stock-category "food")
      (insert-datum-into-table (string-to-keyword stock-category)
        :name name
        :description description
        :amount amount
        :cost-per-package cost-per-package
        :calories-per-package calories-per-package)

      (insert-datum-into-table (string-to-keyword stock-category)
        :name name
        :description description
        :amount amount
        :cost-per-package cost-per-package)))


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
                  (cond ((string-equal |stock-category| "food")
                         (get-datum-by-id :food id))

                        ((string-equal |stock-category| "water")
                         (get-datum-by-id :water id))

                        ((string-equal |stock-category| "medicine")
                         (get-datum-by-id :medicine id))

                        ((string-equal |stock-category| "weapon")
                         (get-datum-by-id :weapon id))))))
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
    (cond ((string-equal stock-category "food")
           (with-connection-execute
             (generate-update-datum-by-id :food
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|
               :calories-per-package |calories-per-package|)))

          ((string-equal stock-category "water")
           (with-connection-execute
             (generate-update-datum-by-id :water
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|)))

          ((string-equal stock-category "medicine")
           (with-connection-execute
             (generate-update-datum-by-id :medicine
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|)))

          ((string-equal stock-category "weapon")
           (with-connection-execute
             (generate-update-datum-by-id :weapon
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|))))
    (redirect
     (format nil "/app/list/~A" stock-category))))

(defroute ("/app/delete/:id" :method :GET) (&key id
                                                 |stock-category|)
  (let ((table-name
         (string-to-keyword |stock-category|)))
    (handler-case
        (delete-datum-from-table table-name id)
      
      (DBI.ERROR:DBI-PROGRAMMING-ERROR (e)
        (log:error
         "~A"
         e))))
  (redirect (format nil "/app/list/~A"
                    |stock-category|)))

;;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
