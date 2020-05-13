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
  (:documentation "Database utilities.")
  (:use :cl)

  (:import-from :alexandria
                :assoc-value)
  (:import-from :uiop
                :read-file-string)
  (:import-from :quaremain.utilities.string
                :string-to-keyword)

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
                :update)
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
               (description :type 'text)
               (amount :type 'integer :not-null t)
               (cost-per-package :type 'real :not-null t)
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
                              cost-per-package
                              &body sxql-column-specifier-forms)
  `(execute
    (update ,table-name
            (set= :name ,name
                  :description ,description

                  :amount ,amount
                  :cost-per-package ,cost-per-package
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


(defun row-exist-by-id? (table-name id)
  (not (null
        (with-connection (db)
          (get-datum-by-id table-name id)))))

(defun row-exist-by-name? (table-name name)
  (not (null
        (with-connection (db)
          (get-datum-by-name table-name name)))))


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
                         :cost-per-package (alexandria:assoc-value item :cost-per-package)
                         :calories-per-package (alexandria:assoc-value item :calories-per-package)))
       
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
             :cost-per-package (alexandria:assoc-value item :cost-per-package)
             :millilitre-per-package (alexandria:assoc-value item :millilitre-per-package)))
       
       json-data))))

(defun medicine-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/medicine.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))

    (with-connection (db)
      
      (mapcar #'(lambda (item)
                  (create-datum :medicine
                    :name (alexandria:assoc-value item :name)
                    :description (alexandria:assoc-value item :description)
                    :amount (alexandria:assoc-value item :amount)
                    :cost-per-package (alexandria:assoc-value item :cost-per-package)))
              
              json-data))))

(defun weapon-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/weapon.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))

    (with-connection (db)
      
      (mapcar #'(lambda (item)                  
                  (create-datum :weapon
                    :name (alexandria:assoc-value item :name)
                    :description (alexandria:assoc-value item :description)
                    :amount (alexandria:assoc-value item :amount)
                    :cost-per-package (alexandria:assoc-value item :cost-per-package)))
              
              json-data))))

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
