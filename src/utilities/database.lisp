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
                :where
                :delete-from)
  (:import-from :quaremain.utilities.config
                :+database-path+)

  ;; Exceptions.
  (:import-from :quaremain.utilities.exception
                :row-doesnt-exist-error)
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
           :get-datum-from-table-by-name
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
               (description :type 'text)
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

(defparameter +seeds-directory+ "seeds/stocks")

(defun food-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/food.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))

    (with-connection (db)
      
      (mapcar
       #'(lambda (item)
           (execute
            (insert-datum-into-table :food
              :name (alexandria:assoc-value item :name)
              :description (alexandria:assoc-value item :description)
              :amount (alexandria:assoc-value item :amount)
              :cost-per-package (alexandria:assoc-value item :cost-per-package)
              :calories-per-package (alexandria:assoc-value item :calories-per-package))))
       
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
           (execute
            (insert-datum-into-table :water
              :name (alexandria:assoc-value item :name)
              :description (alexandria:assoc-value item :description)
              :amount (alexandria:assoc-value item :amount)
              :cost-per-package (alexandria:assoc-value item :cost-per-package)
              :millilitre-per-package (alexandria:assoc-value item :millilitre-per-package))))
       
       json-data))))

(defun medicine-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/medicine.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))

    (with-connection (db)
      
      (mapcar #'(lambda (item)
                  (execute
                   (insert-datum-into-table :medicine
                     :name (alexandria:assoc-value item :name)
                     :description (alexandria:assoc-value item :description)
                     :amount (alexandria:assoc-value item :amount)
                     :cost-per-package (alexandria:assoc-value item :cost-per-package))))
              
              json-data))))

(defun weapon-seed-migrator ()
  (let* ((all-data
          (uiop:read-file-string
           (format nil "~A/weapon.json" +seeds-directory+)))
         (json-data
          (cl-json:decode-json-from-string all-data)))

    (with-connection (db)
      
      (mapcar #'(lambda (item)
                  (execute
                   (insert-datum-into-table :weapon
                     :name (alexandria:assoc-value item :name)
                     :description (alexandria:assoc-value item :description)
                     :amount (alexandria:assoc-value item :amount)
                     :cost-per-package (alexandria:assoc-value item :cost-per-package))))
              
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

(defun get-datum-from-table-by-name (table-name name)
  (with-connection (db)
    (retrieve-one
     (select :* (from table-name)
             (where (:= :name name))))))

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

(defun row-exist-by-id? (table-name id)
  (if (null
       (get-datum-from-table table-name id))
      nil
      t))

(defun row-exist-by-name? (table-name name)
  (if (null
       (get-datum-from-table-by-name table-name name))
      nil
      t))


(defun delete-datum-from-table (table-name id)
  (with-connection (db)
    (if (row-exist-by-id? table-name id)
        (execute
         (delete-from table-name
           (where (:= :id id))))
        (error 'row-doesnt-exist-error
               :table-name table-name
               :id id))))
