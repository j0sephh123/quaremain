;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.web
  (:documentation "Server routing handler.")
  (:use :cl
        :caveman2
        :quaremain.utilities.config
        :quaremain.view)
  
  (:import-from :quaremain.utilities.exception
                :row-doesnt-exist-error
                :row-with-same-name-already-exist-error
                :no-database-tables-to-be-found-error
                :total-required-survival-resources-is-too-low-error
                :user-input-doesnt-satisfy-constraint-error)
  (:import-from :quaremain.utilities.string
                :get-key-value)
  
  (:import-from :quaremain.models.stock.stock
                :create-stock
                :update-stock
                :get-stocks-sum
                :get-coerced-stock-cost-by-id
                :delete-stock-by-id
                :get-all-stocks)
  
  (:import-from :quaremain.models.stock.survival
                :get-total-survival-days
                :survival-days-type)

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

(defroute "/experimental" ()
  (render #p"experimental.html"))

(defparameter +status-codes+
  '((:not-found . 404)
    (:success . 200)))

(defun get-status-code (key)
  (get-key-value +status-codes+ key))

(defun get-cors-headers
    (allow-origin
     allow-methods
     allow-headers)
  (list :access-control-allow-origin allow-origin
        :access-control-allow-methods allow-methods
        :access-control-allow-headers allow-headers))

(defun set-header-origin
    (response
     allow-origin
     allow-methods
     allow-headers)
  (setf (response-headers response)
        (append (response-headers response)
                (get-cors-headers allow-origin
                                  allow-methods
                                  allow-headers))))

(defun cors-handler
    (response
     &key
       (allow-origin "*")                       
       (allow-headers "Content-Type"))
  (set-header-origin
   response
   allow-origin
   "GET, POST, OPTIONS"
   allow-headers))

(defmacro defapi (route params &body body)
  "Convenient macro to define API routes with CORS handling."
  `(defroute ,route ,params
     (cors-handler *response*)
     ,@body))

(defapi "/api/app/list/food"
    (&key
     (|fromRow| "1")
     (|perPage| "10"))
  (let ((food-stocks
         (get-stocks-sum
          :food
          |fromRow|
          |perPage|)))
    (if (null food-stocks)
        (render-json (list
                      :error "No food stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks food-stocks
                      :status (get-status-code :success))))))

(defapi "/api/app/list/water"
    (&key
     (|fromRow| "1")
     (|perPage| "10"))
  (let ((water-stocks
         (get-stocks-sum
          :water
          |fromRow|
          |perPage|)))
    (if (null water-stocks)
        (render-json (list
                      :error "No water stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks water-stocks
                      :status (get-status-code :success))))))

(defapi "/api/app/list/medicine"
    (&key
     (|fromRow| "1")
     (|perPage| "10"))
  (let ((medicine-stocks
         (get-stocks-sum
          :medicine
          |fromRow|
          |perPage|)))
    (if (null medicine-stocks)
        (render-json (list
                      :error "No medicine stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks medicine-stocks
                      :status (get-status-code :success))))))

(defapi "/api/app/list/weapon"
    (&key
     (|fromRow| "1")
     (|perPage| "10"))
  (let ((weapon-stocks
         (get-stocks-sum
          :weapon
          |fromRow|
          |perPage|)))
    (if (null weapon-stocks)
        (render-json (list
                      :error "No weapon stocks available."
                      :status (get-status-code :not-found)))
        (render-json (list
                      :stocks weapon-stocks
                      :status (get-status-code :success))))))

(defapi "/api/app/list/show/:id"
    (&key
     id
     |stockCategory|)
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

(defapi "/api/app/list/create"
    (&key
     |stockCategory|
     |name|
     (|description| "")
     |amount|
     |costPerStock|
     (|caloriesPerStock| "")
     (|millilitresPerStock| ""))
  (handler-case
      (progn
        (create-stock `((:stock-category . ,|stockCategory|)
                        (:name . ,|name|)
                        (:description . ,|description|)
                        (:amount . ,|amount|)
                        (:cost-per-stock . ,|costPerStock|)
                        (:calories-per-stock . ,|caloriesPerStock|)
                        (:millilitres-per-stock . ,|millilitresPerStock|)))
        (render-json (list
                      :status (get-status-code :success)
                      :registered-stock
                      (list
                       :stock-category |stockCategory|
                       :name |name|
                       :description |description|
                       :amount |amount|
                       :cost-per-stock |costPerStock|
                       :calories-per-stock |caloriesPerStock|
                       :millilitres-per-stock |millilitresPerStock|))))

    (row-with-same-name-already-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Stock with same name already exist! Duplication is not allowed!"
                    :status (get-status-code :not-found))))
    (user-input-doesnt-satisfy-constraint-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "User input doesn't satisfy constraint!"
                    :status (get-status-code :not-found))))))

(defapi "/api/app/list/update/:id"
    (&key
     id
     |stockCategory|
     |name|
     (|description| "")
     |amount|
     |costPerStock|
     (|caloriesPerStock| "")
     (|millilitresPerStock| ""))
  (handler-case
      (progn
        (update-stock
         `((:stock-category . ,|stockCategory|)
           (:id . ,id)
           (:name . ,|name|)
           (:description . ,|description|)
           (:amount . ,|amount|)
           (:cost-per-stock . ,|costPerStock|)
           (:calories-per-stock . ,|caloriesPerStock|)
           (:millilitres-per-stock . ,|millilitresPerStock|)))
        (render-json (list
                      :status (get-status-code :success))))
    
    (row-doesnt-exist-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Cannot update stock that doesn't exist!"
                    :status (get-status-code :not-found))))
    (user-input-doesnt-satisfy-constraint-error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "User input doesn't satisfy constraint!"
                    :status (get-status-code :not-found))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "Updating stock list failed!"
                    :status (get-status-code :not-found))))))

(defapi "/api/app/list/delete/:id" (&key id |stockCategory|)
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

(defapi "/api/app/list/get-all-stocks" ()
  (handler-case
      (progn
        (render-json (list
                      :stocks (get-all-stocks)
                      :status (get-status-code
                               :success))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "All stocks are empty!"
                    :status (get-status-code
                             :not-found))))))

(defapi "/api/app/list/reset-database" ()
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

(defapi "/api/app/list/total-survival-days" ()
  (handler-case
      (let ((total-survival-days (get-total-survival-days)))
        (render-json
         (list :total-survival-days
               total-survival-days
               :status (get-status-code
                        :success)
               :survival-alert-type
               (survival-days-type total-survival-days))))
    
    (total-required-survival-resources-is-too-low-error (exception)
      (log:error "~A" exception)
      (render-json
       (list :error "Total required survival resources is too low! Consider stocking more food and water!"
             :status (get-status-code :not-found))))))
