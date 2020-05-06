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
(defpackage quaremain.utilities.database
  (:documentation "Database access handler.")
  (:use :cl)
  (:import-from :cl-dbi
                :connect-cached
                :disconnect)
  (:import-from :datafly
                :execute
                :retrieve-one
                :retrieve-all
                :*connection*)
  (:import-from :sxql
                :create-table
                :drop-table
                :from
                :select
                :where)
  (:import-from :quaremain.utilities.config
                :+database-path+)

  ;; Exceptions.
  (:import-from :sqlite
                :sqlite-error)
  (:import-from :dbi.error
                :dbi-programming-error)
  (:export :db
           :with-connection
           :migrate-tables
           :drop-tables
           :insert-datum-into-table
           :get-all-datum-from-table
           :get-datum-from-table
           :generate-update-datum
           :delete-datum-from-table))
(in-package :quaremain.utilities.database)

(defun db ()
  (connect-cached
   :sqlite3
   :database-name +database-path+))

(defmacro with-connection (conn &body sxql-forms)
  `(let ((*connection* ,conn))
     ,@sxql-forms))

(defmacro deftable (table-name &body sxql-column-specifier-forms)
  "Define a basic base table for new model. Remember that
   this will inherit id, name, description, amount and
   cost-per-package column specifiers as well. Only suitable
   for tables that are related to market products.

   Example: (deftable :user 
              (:username :type 'varchar)
              (:password :type 'text :not-null t))
   "
  `(let ((generated-sxql-schema
          (create-table (,table-name :if-not-exists t)
                        ((id :type 'integer :primary-key t)
                         (name :type 'text :not-null t)
                         (description :type 'text :not-null t)
                         (amount :type 'integer :not-null t)
                         (cost-per-package :type 'real :not-null t)
                         ,@sxql-column-specifier-forms))))
     generated-sxql-schema))

(defun migrate-tables ()
  (handler-case
      (progn
        (log:info "Attempting to migrate all models schemas if not exist.")
        (with-connection (db)
          (mapcar (lambda (generated-sxql-schema)
                    (execute generated-sxql-schema))
                  (list (deftable :food
                          (calories-per-package
                           :type 'integer
                           :not-null t))
                        (deftable :water
                          (millilitre-per-package
                           :type 'integer
                           :not-null t))
                        (deftable :medicine)
                        (deftable :weapon)))))
    
    (sqlite-error (exception)
      (log:error "Are you trying to run from the outside of
                  Quaremain's project directory?")
      (log:error "[SQLITE-ERROR]: ~A" exception)
      (uiop:quit 1))))

(defun drop-tables ()
  (handler-case
      (progn
        (log:info "Attempting to erase all tables from the database")
        (with-connection (db)
          (mapcar (lambda (table-name)
                    (execute
                     (drop-table table-name)))
                  (list :food
                        :water
                        :medicine
                        :weapon)))
        (log:info "Database tables has been erased"))
    
    (dbi-programming-error (exception)
      (log:error "[DBI-PROGRAMMING-ERROR]: ~A" exception)
      (log:error "No existing tables in the database found to be erased"))
    
    (sqlite-error (exception)
      (log:error "[SQLITE-ERROR]: ~A" exception)
      (log:error "Are you trying to run from the outside of
                  Quaremain's project directory?")
      (uiop:quit 1))))

(defmacro insert-datum-into-table (table-name &body key-and-value)
  `(sxql:insert-into ,table-name
     (sxql:set= ,@key-and-value)))

(defun get-all-datum-from-table (table-name)
  (with-connection (db)
    (retrieve-all
     (select :*
       (from table-name)))))

(defun get-datum-from-table (table-name id)
  (with-connection (db)
    (retrieve-one
     (select :* (from table-name)
             (where (:= :id id))))))

(defmacro generate-update-datum (table-name
                                 id
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
  (with-connection (db)
    (execute
     (sxql:delete-from table-name
       (sxql:where (:= :id id))))))
