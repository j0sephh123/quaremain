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

(defparameter *food-model*
  (sxql:create-table (:food :if-not-exists t)
      ((id :type 'integer :primary-key t)
       (name :type 'text :not-null t)
       (description :type 'text)
       (amount :type 'integer :not-null t)
       (cost-per-package :type 'real :not-null t)
       (calories-per-package :type 'integer :not-null t))))

(defparameter *water-model*
  (sxql:create-table (:water :if-not-exists t)
      ((id :type 'integer :primary-key t)
       (name :type 'text :not-null t)
       (description :type 'text)
       (amount :type 'integer :not-null t)
       (cost-per-package :type 'real :not-null t))))

(defmacro with-connection-execute (&body body)
  `(with-connection (db)
     (datafly:execute ,@body)))


(defun migrate-models ()
  "Returns list of nils if operation succeed."
  (with-connection (db)
    (mapcar (lambda (model)
              (datafly:execute model))
            (list *food-model*
                  *water-model*))))

(defun drop-models ()
  "Returns list of nils if operation succeed."
  (with-connection (db)
    (mapcar (lambda (table)
              (datafly:execute
               (sxql:drop-table table)))
            (list :food :water))))


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

(defroute "**" ()
  (setf (getf (response-headers *response*) :cache-control) "no-cache, no-store, must-revalidate")
  (next-route))

(defroute "/" ()
  (render #p"index.html"
          `(:food-list ,(sum-all-cost-per-package
                         (get-all-from-model :food)))))

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
  (coerce (getf datum :cost-per-package)
          'single-float)
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
