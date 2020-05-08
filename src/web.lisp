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
  (render #p"app/list.html"
          `(:data ,(sum-stocks-from-table :food)
                  :list-type "food")))


;;; EXPERIMENTAL ENDPOINTS

;;; Experimental client-side index page
(defroute "/experimental" ()
  (render #p"experimental.html"))

;; (defroute "/api/0.1/os/")
;; for OS commands calls

;;; GET/SHOW/LIST

(defstruct status-code
  (not-found 404)
  (success 200)
  (redirect 302))

(defparameter +status-code-definition+ (make-status-code))

(defroute "/api/0.1/app/list/food" ()
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

;; FIXME: amount is not received
;; workaround, uses stock-amount as parameter key
;; for route
(defroute ("/api/0.1/app/list/create" :method :POST) (&key
                                                      |stock-category|
                                                      |stock-amount|
                                                      |name|
                                                      |description|
                                                      |cost-per-package|
                                                      |calories-per-package|
                                                      |millilitre-per-package|)
  (handler-case
      (progn
        (create-new-stock :stock-category |stock-category|
                          :name |name|
                          :description |description|
                          :amount |stock-amount|
                          :cost-per-package |cost-per-package|
                          :calories-per-package |calories-per-package|
                          :millilitre-per-package |millilitre-per-package|)
        (render-json (list
                      :status (status-code-success
                               +status-code-definition+))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error "New stock list creation failed."
                    :status (status-code-not-found
                             +status-code-definition+))))))

(defroute "/api/0.1/app/list/delete/:id" (&key id |stock-category|)
  (handler-case
      (progn
        ;; actually returns nothing even if id is not exist?
        (delete-stock-by-category-and-id |stock-category| id)
        (render-json (list
                      :status (status-code-success
                               +status-code-definition+))))
    (error (exception)
      (log:error "~A" exception)
      (render-json (list
                    :error exception
                    :status (status-code-not-found
                             +status-code-definition+))))))

;;; EXPERIMENTAL ENDPOINTS

(defroute "/about" ()
  (render #p"about.html"))


(defroute "/app/list/food" ()
  (render #p"app/list.html"
          `(:data ,(sum-stocks-from-table :food)
                  :list-type "food")))

(defroute "/app/list/water" ()
  (render #p"app/list.html"
          `(:data ,(sum-stocks-from-table :water)
                  :list-type "water")))

(defroute "/app/list/medicine" ()
  (render #p"app/list.html"
          `(:data ,(sum-stocks-from-table :medicine)
                  :list-type "medicine")))

(defroute "/app/list/weapon" ()
  (render #p"app/list.html"
          `(:data ,(sum-stocks-from-table :weapon)
                  :list-type "weapon")))

(defroute "/app/create-form" ()
  (render #p"app/create-form.html"))

(defroute ("/app/create" :method :POST) (&key
                                         |stock-category|
                                         |name|
                                         |description|
                                         |amount|
                                         |cost-per-package|
                                         |calories-per-package|
                                         |millilitre-per-package|)
  (handler-case
      (progn
        (create-new-stock :stock-category |stock-category|
                          :name |name|
                          :description |description|
                          :amount |amount|
                          :cost-per-package |cost-per-package|
                          :calories-per-package |calories-per-package|
                          :millilitre-per-package |millilitre-per-package|)
        (redirect
         (format nil "/app/list/~A" |stock-category|)))
    (error (exception)
      (log:error "~A" exception)
      (redirect (format nil "/app/list/~A" |stock-category|)))))

(defroute "/app/update-form/:id" (&key id
                                       |stock-category|)
  (setf (gethash 'session-stock-id *session*) id)
  (setf (gethash 'session-stock-category *session*) |stock-category|)

  (render #p"app/update-form.html"
          (let ((coerced-stock
                 (get-coerced-stock-by-category-and-id |stock-category| id)))
            (list :datum coerced-stock
                  :list-type |stock-category|))))

(defroute ("/app/update" :method :POST) (&key |name|
                                              |description|
                                              |amount|
                                              |cost-per-package|
                                              |calories-per-package|
                                              |millilitre-per-package|)

  (let* ((id (gethash 'session-stock-id *session*))
         (stock-category
          (gethash 'session-stock-category *session*)))
    (update-stock-by-category-and-id stock-category
                                     id
                                     |name|
                                     |description|
                                     |amount|
                                     |cost-per-package|
                                     |calories-per-package|
                                     |millilitre-per-package|)
    (redirect
     (format nil "/app/list/~A" stock-category))))

(defroute ("/app/delete/:id" :method :GET) (&key id
                                                 |stock-category|)
  
  (delete-stock-by-category-and-id |stock-category| id)
  (redirect (format nil "/app/list/~A"
                    |stock-category|)))
