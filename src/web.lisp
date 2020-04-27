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

;;;; quaremain.web - Server routing handler.

(in-package :cl-user)
(defpackage quaremain.web
  (:use :cl
        :caveman2
        :quaremain.config
        :quaremain.view)
  (:import-from :quaremain.db
                :db
                :with-connection
                :with-connection-execute)
  (:export :*web*))
(in-package :quaremain.web)

;;; Application.

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(defparameter *session* (make-hash-table))
(clear-routing-rules *web*)

(defmacro insert-datum (model-table &rest key-val)
  `(with-connection-execute
     (sxql:insert-into ,model-table
       (sxql:set= ,@key-val))))

(defun get-all-from-model (model-table)
  (with-connection (db)
    (datafly:retrieve-all
     (sxql:select :*
       (sxql:from model-table)))))

(defun sum-all-cost-per-package (data)
  (loop for datum in data
     do (setf (getf datum :cost-per-package)
              (coerce (* (getf datum :amount)
                         (getf datum :cost-per-package))
                      'single-float)))
  data)

(defun sum-all-calories-per-package (data)
  (loop for datum in data
     do (setf (getf datum :calories-per-package)
              (* (getf datum :amount)
                 (getf datum :calories-per-package))))
  data)

(defun get-datum-by-id (model-table id)
  (with-connection (db)
    (datafly:retrieve-one
     (sxql:select :* (sxql:from model-table)
                  (sxql:where (:= :id id))))))

(defmacro generate-update-datum-by-id (model-table id
                                       name
                                       description
                                       amount
                                       cost-per-package
                                       &body body)
  `(sxql:update ,model-table
     (sxql:set= :name ,name
                :description ,description
                :amount ,amount
                :cost-per-package ,cost-per-package
                ,@body)
     (sxql:where (:= :id ,id))))

(defun delete-datum-from-model (model-table id)
  (with-connection-execute
    (sxql:delete-from model-table
      (sxql:where (:= :id id)))))


;;; Routing rules.
(defroute "/" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-calories-per-package
                    (sum-all-cost-per-package
                     (get-all-from-model :food)))
                  :list-type "food")))

(defroute "/app/list/food" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-calories-per-package
                    (sum-all-cost-per-package
                     (get-all-from-model :food)))
                  :list-type "food")))

(defroute "/app/list/water" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :water))
                  :list-type "water")))

(defroute "/app/list/medicine" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :medicine))
                  :list-type "medicine")))

(defroute "/app/list/weapon" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :weapon))
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

  (cond ((string-equal |stock-category| "food")
         (insert-datum :food
                       :name |name|
                       :description |description|
                       :amount |amount|
                       :cost-per-package |cost-per-package|
                       :calories-per-package |calories-per-package|)
         (redirect "/app/list/food"))

        ((string-equal |stock-category| "water")
         (insert-datum :water
                       :name |name|
                       :description |description|
                       :amount |amount|
                       :cost-per-package |cost-per-package|)
         (redirect "/app/list/water"))

        ((string-equal |stock-category| "medicine")
         (insert-datum :medicine
                       :name |name|
                       :description |description|
                       :amount |amount|
                       :cost-per-package |cost-per-package|)
         (redirect "/app/list/medicine"))

        ((string-equal |stock-category| "weapon")
         (insert-datum :weapon
                       :name |name|
                       :description |description|
                       :amount |amount|
                       :cost-per-package |cost-per-package|)
         (redirect "/app/list/weapon"))))

(defun coerce-cost-per-package (datum)
  (let ((cost-per-package (getf datum :cost-per-package)))
    (setf (getf datum :cost-per-package)
          (coerce cost-per-package 'single-float)))
  datum)

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
               :calories-per-package |calories-per-package|))
           (redirect "/app/list/food"))

          ((string-equal stock-category "water")
           (with-connection-execute
             (generate-update-datum-by-id :water
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|))
           (redirect "/app/list/water"))

          ((string-equal stock-category "medicine")
           (with-connection-execute
             (generate-update-datum-by-id :medicine
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|))
           (redirect "/app/list/medicine"))

          ((string-equal stock-category "weapon")
           (with-connection-execute
             (generate-update-datum-by-id :weapon
                 id
                 |name|
                 |description|
                 |amount|
                 |cost-per-package|))
           (redirect "/app/list/weapon")))))

(defroute ("/app/delete/:id" :method :GET) (&key id
                                                 |stock-category|)
  (let ((model-table
         (read-from-string
          (format nil ":~a" |stock-category|))))
    (handler-case
        (delete-datum-from-model model-table id)
      
      (DBI.ERROR:DBI-PROGRAMMING-ERROR (e)
        (format nil
                "~a~a" "Error: list is not exist to be deleted."
                e))))
  (redirect (format nil "/app/list/~a"
                    |stock-category|)))

;;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
