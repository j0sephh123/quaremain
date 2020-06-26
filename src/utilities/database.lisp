;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.utilities.database
  (:documentation "Database utilities.")
  (:use :cl)

  (:import-from :alexandria
                :assoc-value)
  (:import-from :uiop
                :read-file-string)
  (:import-from :quaremain.utilities.string
                :string->keyword)

  (:import-from :cl-json
                :decode-json-from-string)
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
                :where
                :delete-from
                :insert-into
                :set=
                :update
                :order-by
                :limit)
  (:import-from :quaremain.utilities.config
                :+database-path+
                :+seeds-directory+)

  ;; Exceptions.
  (:import-from :quaremain.utilities.exception
                :no-database-tables-to-be-found-error
                :row-doesnt-exist-error)
  (:import-from :sqlite
                :sqlite-error)
  (:import-from :dbi.error
                :dbi-programming-error)
  (:export :db
           :with-connection
           :migrate-tables
           :drop-tables
           :create-datum
           :get-all-paginated-datum
           :get-all-datum
           :get-datum-by-id
           :get-datum-by-name
           :update-datum-by-id
           :delete-datum-by-id))
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
   cost-per-stock column specifiers as well. Only suitable
   for tables that are related to market products.

   Example: (deftable :user 
              (:username :type 'varchar)
              (:password :type 'text :not-null t))
   "
  `(let ((generated-sxql-schema
          (create-table (,table-name :if-not-exists t)
              ((id :type 'integer :primary-key t)
               (name :type 'text :not-null t)
               (description :type 'text)
               (amount :type 'integer :not-null t)
               (cost-per-stock :type 'real :not-null t)
               ,@sxql-column-specifier-forms))))
     generated-sxql-schema))

(defun migrate-tables ()
  (handler-case
      (progn
        (log:info "Attempting to migrate all tables schemas if not exist.")
        (with-connection (db)
          (mapcar (lambda (generated-sxql-schema)
                    (execute generated-sxql-schema))
                  (list (deftable :food
                          (calories-per-stock
                           :type 'integer
                           :not-null t))
                        (deftable :water
                          (millilitres-per-stock
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
      (log:error "No existing tables in the database found to be erased")
      (error 'no-database-tables-to-be-found-error))
    (sqlite-error (exception)
      (log:error "[SQLITE-ERROR]: ~A" exception)
      (log:error "Are you trying to run from the outside of
                  Quaremain's project directory?")
      (uiop:quit 1))))

(defmacro create-datum (table-name &body key-and-value)
  `(execute
    (insert-into ,table-name
      (set= ,@key-and-value))))

(defmacro get-all-paginated-datum
    (table-name
     &optional
       (from-row 1)
       (per-page 10))
  `(retrieve-all
    (select :*
      (from ,table-name)
      (where (:>= :id ,from-row))
      (order-by :id)
      (limit ,per-page))))

(defmacro get-all-datum (table-name)
  `(retrieve-all
    (select :*
      (from ,table-name))))

(defmacro get-datum-by-id (table-name id)
  `(retrieve-one
    (select :* (from ,table-name)
            (where (:= :id ,id)))))

(defmacro get-datum-by-name (table-name name)
  `(retrieve-one
    (select :* (from ,table-name)
            (where (:= :name ,name)))))

(defmacro update-datum-by-id (table-name
                              id
                              name
                              description
                              amount
                              cost-per-stock
                              &body sxql-column-specifier-forms)
  `(execute
    (update ,table-name
            (set= :name ,name
                  :description ,description

                  :amount ,amount
                  :cost-per-stock ,cost-per-stock
                  ,@sxql-column-specifier-forms)
            (where (:= :id ,id)))))

(defmacro delete-datum-by-id (table-name id)
  `(execute
    (delete-from ,table-name
                 (where (:= :id ,id)))))

(defun row-exist-by-id? (table-name id)
  (not (null
        (with-connection (db)
          (get-datum-by-id table-name id)))))

(defun row-exist-by-name? (table-name name)
  (not (null
        (with-connection (db)
          (get-datum-by-name table-name name)))))

(defun seeds-migrator (seed-category)
  (let* ((seeds
          (read-file-string
           (format nil
                   "~A/~A.json"
                   +seeds-directory+
                   seed-category)))
         (parsed-from-json-seeds
          (decode-json-from-string seeds)))
    (with-connection (db)
      (mapcar
       #'(lambda (seed)
           (create-datum (string->keyword seed-category)
             :name (assoc-value seed :name)
             :description (assoc-value seed :description)
             :amount (assoc-value seed :amount)
             :cost-per-stock (assoc-value seed :cost-per-stock)))
       parsed-from-json-seeds))))

(defun food-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/food.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))
    (with-connection (db)
      (mapcar
       #'(lambda (item)
           (create-datum :food
             :name (alexandria:assoc-value item :name)
             :description (alexandria:assoc-value item :description)
             :amount (alexandria:assoc-value item :amount)
             :cost-per-stock (alexandria:assoc-value item :cost-per-stock)
             :calories-per-stock (alexandria:assoc-value item :calories-per-stock)))
       
       json-data))))

(defun water-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/water.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))
    (with-connection (db)      
      (mapcar
       #'(lambda (item)
           (create-datum :water
             :name (alexandria:assoc-value item :name)
             :description (alexandria:assoc-value item :description)
             :amount (alexandria:assoc-value item :amount)
             :cost-per-stock (alexandria:assoc-value item :cost-per-stock)
             :millilitres-per-stock (alexandria:assoc-value item :millilitres-per-stock)))
       json-data))))

(defun medicine-seed-migrator ()
  (seeds-migrator "medicine"))

(defun weapon-seed-migrator ()
  (seeds-migrator "weapon"))

(defun migrate-seeds ()
  (handler-case
      (progn
        (log:info "Migrating seeds...")
        (food-seed-migrator)
        (water-seed-migrator)
        (medicine-seed-migrator)
        (weapon-seed-migrator)
        (log:info "Seeds migration complete"))
    (error (exception)
      (log:error exception)
      (log:error "Failed to migrate database seeds.."))))
