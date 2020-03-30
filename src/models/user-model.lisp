(in-package :cl-user)
(defpackage quaremain.models.user-model
  (:use :cl)
  (:import-from :quaremain.db
                :db
                :with-connection)
  (:import-from :datafly
                :execute
                :retrieve-one)
  (:import-from :sxql
                :create-table))
(in-package :quaremain.models.user-model)


(defun create-user-table ()
  (with-connection (db)
    (execute
     (create-table (:user :if-not-exists t)
         ((id :type 'integer :primary-key t)
          (username :type 'string :not-null t)
          (email :type 'string :not-null t))))))
