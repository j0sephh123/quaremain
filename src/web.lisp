(in-package :cl-user)
(defpackage quaremain.web
  (:use :cl
        :caveman2
        :quaremain.config
        :quaremain.view)
  (:import-from :quaremain.db
                :with-connection
                :db)
  (:export :*web*
           :migrate-models))
(in-package :quaremain.web)

;;; Application.

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(defparameter *session* (make-hash-table))
(clear-routing-rules *web*)

(defmacro deftable (identifier table-name &body body)
  "Define a basic base table for new model. This will create a
   new non-dynamic global variable using the supplied first parameter
   as the actual identifier name.
   "
  `(defparameter ,identifier
     (sxql:create-table (,table-name :if-not-exists t)
         ((id :type 'integer :primary-key t)
          (name :type 'text :not-null t)
          (description :type 'text :not-null t)
          (amount :type 'integer :not-null t)
          (cost-per-package :type 'real :not-null t)
          ,@body))))

;; Models.
(deftable *food* :food
  (calories-per-package :type 'integer :not-null t))
(deftable *water* :water)
(deftable *medicine* :medicine)

(defmacro with-connection-execute (&body body)
  `(with-connection (db)
     (datafly:execute ,@body)))


(defun migrate-models ()
  "Returns list of nils if operation succeed."
  (with-connection (db)
    (mapcar (lambda (model)
              (datafly:execute model))
            (list *food*
                  *water*
                  *medicine*))))

(defun drop-models ()
  "Returns list of nils if operation succeed."
  (with-connection (db)
    (mapcar (lambda (table)
              (datafly:execute
               (sxql:drop-table table)))
            (list :food
                  :water
                  :medicine))))


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

(defun get-datum-by-id (model-table id)
  (with-connection (db)
    (datafly:retrieve-one
     (sxql:select :* (sxql:from model-table)
                  (sxql:where (:= :id id))))))

(defun update-datum-by-id (model-table id &key
                                            name
                                            description
                                            amount
                                            cost-per-package
                                            calories-per-package)
  (with-connection-execute
      (sxql:update model-table
        (sxql:set= :name name
                   :description description
                   :amount amount
                   :cost-per-package cost-per-package
                   :calories-per-package calories-per-package)
        (sxql:where (:= :id id)))))

(defun delete-datum-from-model (model-table id)
  (with-connection-execute
    (sxql:delete-from model-table
      (sxql:where (:= :id id)))))


;;; Routing rules.
(defroute "/" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :food))
                  :list-type "food")))

(defroute "/app/list/food" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :food))
                  :list-type "food")))

(defroute "/app/list/water" ()
  (render #p"app/list.html"
          `(:data ,(sum-all-cost-per-package
                    (get-all-from-model :water))
                  :list-type "water")))

(defroute "/about" ()
  (render #p"about.html"))

(defroute "/app/create-form" ()
  (render #p"app/create-form.html"))

(defroute ("/app/create" :method :POST) (&key |name|
                                              |description|
                                              |amount|
                                              |cost-per-package|
                                              |calories-per-package|)
  (insert-datum :food
                :name |name|
                :description |description|
                :amount |amount|
                :cost-per-package |cost-per-package|
                :calories-per-package |calories-per-package|)
  (redirect "/"))

(defun coerce-cost-per-package (datum)
  (let ((cost-per-package (getf datum :cost-per-package)))
    (setf (getf datum :cost-per-package)
          (coerce cost-per-package 'single-float)))
  datum)

(defroute "/app/update-form/:id" (&key id)
  (setf (gethash 'datum-id *session*) id)
  (render #p"app/update-form.html"
          (let ((coerced-datum (coerce-cost-per-package
                                (get-datum-by-id :food id))))
            (list :datum coerced-datum))))

(defroute ("/app/update" :method :POST) (&key |name|
                                              |description|
                                              |amount|
                                              |cost-per-package|
                                              |calories-per-package|)
  (update-datum-by-id :food
                      (gethash 'datum-id *session*)
                      :name |name|
                      :description |description|
                      :amount |amount|
                      :cost-per-package |cost-per-package|
                      :calories-per-package |calories-per-package|)
  (redirect "/"))

(defroute ("/app/delete/:id" :method '(:GET :DELETE)) (&key id)
  (delete-datum-from-model :food id)
  (redirect "/"))

;;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
