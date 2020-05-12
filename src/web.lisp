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
  (:import-from :quaremain.utilities.exception
                :stock-missing-property-value-error
                :row-doesnt-exist-error
                :row-with-same-name-already-exist-error)
  (:import-from :quaremain.models.stock.stock
                :create-new-stock
                :update-stock-by-category-and-id
                :sum-stocks-from-table
                :get-coerced-stock-by-category-and-id
                :delete-stock-by-category-and-id)
  (:export :*web*))
(in-package :quaremain.web)

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>)
  "Quaremain's system wide application instance.")
(clear-routing-rules *web*)


(defparameter *session* (make-hash-table))

(defroute "/" ()
  (render #p"experimental.html"))

(defroute "/experimental" ()
  (render #p"experimental.html"))

;; (defroute "/api/0.1/os/")
;; for OS commands calls

(defstruct status-code
  (not-found 404)
  (success 200)
  (redirect 302))

(defparameter +status-code-definition+ (make-status-code))


(defun get-cors-headers (allow-origin allow-methods allow-headers)
  (list :access-control-allow-origin allow-origin
        :access-control-allow-methods allow-methods
        :access-control-allow-headers allow-headers))

(defun set-header-origin (response allow-origin allow-methods allow-headers)
  (setf (response-headers response)
        (append (response-headers response)
                (get-cors-headers allow-origin
                                  allow-methods
                                  allow-headers))))

(defun cors-handler (response
                     &key
                       (allow-origin "*")                       
                       (allow-headers "Content-Type"))
  "Handling CORS requests. By default, accepts 'any' origin."

  (set-header-origin response allow-origin "GET, POST, OPTIONS" allow-headers))

(defroute "/api/0.1/app/list/food" ()
  (cors-handler *response*)
  (let ((food-stocks (sum-stocks-from-table :food)))
    (if (null food-stocks)
        (render-json (list
                      :error "No food stocks available."
                      :status (status-code-not-found +status-code-definition+)))
        (render-json (list
                      :stocks food-stocks
                      :status (status-code-success
                               +status-code-definition+))))))

(defroute "/api/0.1/app/list/water" ()
  (cors-handler *response*)
  (let ((water-stocks (sum-stocks-from-table :water)))
    (if (null water-stocks)
        (render-json (list
                      :error "No water stocks available."
                      :status (status-code-not-found
                               +status-code-definition+)))
        (render-json (list
                      :stocks water-stocks
                      :status (status-code-success
                               +status-code-definition+))))))

(defroute "/api/0.1/app/list/medicine" ()
  (cors-handler *response*)
  (let ((medicine-stocks (sum-stocks-from-table :medicine)))
    (if (null medicine-stocks)
        (render-json (list
                      :error "No medicine stocks available."
                      :status (status-code-not-found
                               +status-code-definition+)))
        (render-json (list
                      :stocks medicine-stocks
                      :status (status-code-success
                               +status-code-definition+))))))

(defroute "/api/0.1/app/list/weapon" ()
  (cors-handler *response*)
  (let ((weapon-stocks (sum-stocks-from-table :weapon)))
    (if (null weapon-stocks)
        (render-json (list
                      :error "No weapon stocks available."
                      :status (status-code-not-found
                               +status-code-definition+)))
        (render-json (list
                      :stocks weapon-stocks
                      :status (status-code-success
                               +status-code-definition+))))))

(defroute "/api/0.1/app/list/show/:id" (&key
                                        id
                                        |stockCategory|)
  (handler-case
      (let ((stock
             (get-coerced-stock-by-category-and-id
              |stockCategory|
              id)))
        (render-json (list
                      :stock (list stock)
                      :status (status-code-success
                               +status-code-definition+))))
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (log:error "Row doesn't exist to retrieved!")
      (render-json (list
                    :error "Row doesn't exist to be retrieved!"
                    :status (status-code-not-found
                             +status-code-definition+))))
    (error (exception)
      (log:error "~A" exception)
      (log:error "There was an error when executing this process!")
      (render-json (list
                    :error "There was an error when executing this process!"
                    :status (status-code-not-found
                             +status-code-definition+))))))

;; FIXME: amount is not received
;; workaround, uses stock-amount as parameter key
;; for route
(defroute "/api/0.1/app/list/create" (&key
                                      |stockCategory|
                                      |stockAmount|
                                      |name|
                                      |description|
                                      |costPerPackage|
                                      |caloriesPerPackage|
                                      |millilitrePerPackage|)
  (cors-handler *response*)
  (handler-case
      (progn
        (create-new-stock :stock-category |stockCategory|
                          :name |name|
                          :description |description|
                          :amount |stockAmount|
                          :cost-per-package |costPerPackage|
                          :calories-per-package |caloriesPerPackage|
                          :millilitre-per-package |millilitrePerPackage|)
        (render-json (list
                      :status (status-code-success
                               +status-code-definition+)
                      :registered-stock
                      (list
                       :stock-category |stockCategory|
                       :name |name|
                       :description |description|
                       :amount |stockAmount|
                       :cost-per-package |costPerPackage|
                       :calories-per-package |caloriesPerPackage|
                       :millilitre-per-package |millilitrePerPackage|))))
    (stock-missing-property-value-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Stock is missing unique property value!"
                    :status (status-code-not-found
                             +status-code-definition+))))

    (row-with-same-name-already-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Stock with same name already exist! Duplication is not allowed!"
                    :status (status-code-not-found
                             +status-code-definition+))))))

(defroute "/api/0.1/app/list/update/:id" (&key
                                          id
                                          |stockCategory|
                                          |stockAmount|
                                          |name|
                                          |description|
                                          |costPerPackage|
                                          |caloriesPerPackage|
                                          |millilitrePerPackage|)
  (cors-handler *response*)
  (handler-case
      (progn
        (update-stock-by-category-and-id :stock-category |stockCategory|
                                         :id id
                                         :name |name|
                                         :description |description|
                                         :amount |stockAmount|
                                         :cost-per-package |costPerPackage|
                                         :calories-per-package |caloriesPerPackage|
                                         :millilitre-per-package |millilitrePerPackage|)
        (render-json (list
                      :status (status-code-success
                               +status-code-definition+))))
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Cannot update stock that doesn't exist!"
                    :status (status-code-not-found
                             +status-code-definition+))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Updating stock list failed."
                    :status (status-code-not-found
                             +status-code-definition+))))))

(defroute "/api/0.1/app/list/delete/:id" (&key id |stockCategory|)
  (cors-handler *response*)
  (handler-case
      (progn
        ;; actually returns nothing even if id is not exist?
        (delete-stock-by-category-and-id |stockCategory| id)
        (render-json (list
                      :status (status-code-success
                               +status-code-definition+))))
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Item doesn't exist to be deleted!"
                    :status (status-code-not-found
                             +status-code-definition+))))))
