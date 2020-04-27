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

;;;; quaremain.db - Database access.

(in-package :cl-user)
(defpackage quaremain.db
  (:use :cl)
  (:import-from :cl-dbi
                :connect-cached
                :disconnect)
  (:import-from :datafly
                :execute
                :*connection*)
  (:import-from :sxql
                :create-table)
  (:import-from :quaremain.config
                :*database-name*)
  (:export :db
           :with-connection
           :with-connection-execute
           :deftable))
(in-package :quaremain.db)



(defun db ()
  "Database connection instance."
  (connect-cached
   :sqlite3
   :database-name *database-name*))

(defmacro with-connection (connection &body body)
  "Wraps connection call to the database."
  `(let ((*connection* ,connection))
     (unwind-protect (progn ,@body)
       (disconnect *connection*))))

(defmacro with-connection-execute (&body body)
  "Database connection wrapper which executes SXQL statements
   on call.
   "
  `(with-connection (db)
     (execute ,@body)))

(defmacro deftable (table-name &body body)
  "Define a basic base table for new model. This will
   return the SXQL generated schema statements for executions.
   "
  `(let ((schema
          (create-table (,table-name :if-not-exists t)
              ((id :type 'integer :primary-key t)
               (name :type 'text :not-null t)
               (description :type 'text :not-null t)
               (amount :type 'integer :not-null t)
               (cost-per-package :type 'real :not-null t)
               ,@body))))
     schema))
