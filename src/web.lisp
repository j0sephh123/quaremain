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
                :row-with-same-name-already-exist-error
                :no-database-tables-to-be-found-error
                :total-required-survival-resources-is-too-low-error)
  
  (:import-from :quaremain.models.stock.stock
                :create-stock
                :update-stock-by-id
                :get-stocks-sum
                :get-coerced-stock-cost-by-id
                :delete-stock-by-id
                :get-total-survival-days
                :get-all-stocks)

  (:import-from :quaremain.utilities.database
                :migrate-tables
                :drop-tables)
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

;; (defroute "/api/os/")
;; for OS commands calls

(defparameter +status-codes+
  '((:not-found 404)
    (:success 200)))

(defun get-status-code (key)
  (second (assoc key +status-codes+)))

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

(defroute "/api/app/list/food" ()
  (cors-handler *response*)
  (let ((food-stocks (get-stocks-sum :food)))
    (if (null food-stocks)
        (render-json (list
                      :error "No food stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks food-stocks
                      :status (get-status-code :success))))))

(defroute "/api/app/list/water" ()
  (cors-handler *response*)
  (let ((water-stocks (get-stocks-sum :water)))
    (if (null water-stocks)
        (render-json (list
                      :error "No water stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks water-stocks
                      :status (get-status-code :success))))))

(defroute "/api/app/list/medicine" ()
  (cors-handler *response*)
  (let ((medicine-stocks (get-stocks-sum :medicine)))
    (if (null medicine-stocks)
        (render-json (list
                      :error "No medicine stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks medicine-stocks
                      :status (get-status-code :success))))))

(defroute "/api/app/list/weapon" ()
  (cors-handler *response*)
  (let ((weapon-stocks (get-stocks-sum :weapon)))
    (if (null weapon-stocks)
        (render-json (list
                      :error "No weapon stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks weapon-stocks
                      :status (get-status-code :success))))))

(defroute "/api/app/list/show/:id" (&key
                                    id
                                    |stockCategory|)
  (cors-handler *response*)
  (handler-case
      (let ((stock
             (get-coerced-stock-cost-by-id
              |stockCategory|
              id)))
        (render-json (list
                      :stock (list stock)
                      :status (get-status-code :success))))
    
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (log:error "Row doesn't exist to retrieved!")
      (render-json (list
                    :error "Row doesn't exist to be retrieved!"
                    :status (get-status-code :not-found))))
    (error (exception)
      (log:error "~A" exception)
      (log:error "There was an error when executing this process!")
      (render-json (list
                    :error "There was an error when executing this process!"
                    :status (get-status-code :not-found))))))

;; FIXME: amount is not received
;; workaround, uses stock-amount as parameter key
;; for route
(defroute "/api/app/list/create" (&key
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
        (create-stock :stock-category |stockCategory|
                      :name |name|
                      :description |description|
                      :amount |stockAmount|
                      :cost-per-package |costPerPackage|
                      :calories-per-package |caloriesPerPackage|
                      :millilitre-per-package |millilitrePerPackage|)
        (render-json (list
                      :status (get-status-code :success)
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
                    :status (get-status-code :not-found))))

    (row-with-same-name-already-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Stock with same name already exist! Duplication is not allowed!"
                    :status (get-status-code :not-found))))))

(defroute "/api/app/list/update/:id" (&key
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
        
        (update-stock-by-id :stock-category |stockCategory|
                            :id id
                            :name |name|
                            :description |description|
                            :amount |stockAmount|
                            :cost-per-package |costPerPackage|
                            :calories-per-package |caloriesPerPackage|
                            :millilitre-per-package |millilitrePerPackage|)
        (render-json (list
                      :status (get-status-code :success))))
    
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Cannot update stock that doesn't exist!"
                    :status (get-status-code :not-found))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Updating stock list failed."
                    :status (get-status-code :not-found))))))

(defroute "/api/app/list/delete/:id" (&key id |stockCategory|)
  (cors-handler *response*)
  (handler-case
      (progn
        
        (delete-stock-by-id |stockCategory| id)
        (render-json (list
                      :status (get-status-code
                               :success))))
    
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Item doesn't exist to be deleted!"
                    :status (get-status-code
                             :not-found))))))

(defroute "/api/app/list/get-all-stocks" ()
  (cors-handler *response*)
  (handler-case
      (progn

        (render-json (list
                      :stocks (get-all-stocks)
                      :status (get-status-code
                               :success))))

    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Stocks not found!"
                    :status (get-status-code
                             :not-found))))))

(defroute "/api/app/list/reset-database" ()
  (cors-handler *response*)
  (handler-case
      (progn
        
        (drop-tables)
        (migrate-tables)
        (render-json (list
                      :status (get-status-code
                               :success))))

    (no-database-tables-to-be-found-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "No existing tables to be deleted!"
                    :status (get-status-code
                             :not-found))))
    (error (exception)
      (log:error "~A" exception)
      (log:error "There was something wrong when resetting the database!")
      (render-json (list
                    :error "There was something wrong when resetting the database!"
                    :status (get-status-code
                             :not-found))))))

(defun get-total-survival-days-alert-type (total-survival-days)
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

(defroute "/api/app/list/total-survival-days" ()
  (cors-handler *response*)
  (handler-case
      (let ((total-survival-days (get-total-survival-days)))
        
        (render-json
         (list :total-survival-days
               total-survival-days
               :status (get-status-code
                        :success)
               :survival-alert-type
               (get-total-survival-days-alert-type total-survival-days))))
    
    (total-required-survival-resources-is-too-low-error (exception)
      (log:error "~A" exception)
      (render-json
       (list :error "Total required survival resources is too low! Consider stocking more food and water!"
             :status (get-status-code :not-found))))))
