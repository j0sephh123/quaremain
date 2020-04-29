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
(defpackage quaremain.db
  (:documentation "Database access handler.")
  (:use :cl)
  (:import-from :cl-dbi
                :connect-cached
                :disconnect)
  (:import-from :datafly
                :execute
                :*connection*)
  (:import-from :sxql
                :create-table
                :drop-table)
  (:import-from :quaremain.config
                :+database-path+)

  ;; Exceptions.
  (:import-from :sqlite
                :sqlite-error)
  (:import-from :dbi.error
                :dbi-programming-error)
  (:export :db
           :with-connection
           :with-connection-execute
           :migrate-tables
           :drop-tables
           :insert-datum-into-table
           :get-all-datum-from-table))
(in-package :quaremain.db)

(defun db ()
  "Database init connection.

   returns: database connection instance"
  (connect-cached
   :sqlite3
   :database-name +database-path+))

(defmacro with-connection (database-connection &body sxql-forms)
  `(let ((*connection* ,database-connection))
     (unwind-protect (progn ,@sxql-forms)
       (disconnect *connection*))))

(defmacro with-connection-execute (&body sxql-forms)
  `(with-connection (db)
     (execute ,@sxql-forms)))


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
                        (deftable :water)
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
  `(with-connection-execute
     (sxql:insert-into ,table-name
       (sxql:set= ,@key-and-value))))

(defun get-all-datum-from-table (table-name)
  (with-connection (db)
    (datafly:retrieve-all
     (sxql:select :*
       (sxql:from table-name)))))
