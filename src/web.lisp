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

(defmacro insert-dao (model-table &body body)
  "Example: (insert-dao :water :amount 3 :cost-per-package 92392.3)"
  `(with-connection (db)
     (datafly:execute
      (sxql:insert-into ,model-table
                        (sxql:set= ,@body)))))

(defun get-all-from-model (model-table)
  (with-connection (db)
    (datafly:retrieve-all
     (sxql:select :*
                  (sxql:from model-table)))))

(defun sum-model (datum)
  (loop for item in datum
     do (setf (getf item :cost-per-package)
              (* (getf item :amount)
                 (getf item :cost-per-package))))
  datum)


;;; Routing rules.

(defroute "/" ()
  "By default, shows list of current accumulated stocks."
  (render #p"index.html"
          `(:food-list ,(sum-model
                         (get-all-from-model :food)))))

(defroute "/about" ()
  (render #p"about.html"))


;;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
