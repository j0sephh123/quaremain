;; Copyright 2020 Momozor

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(in-package :cl-user)
(defpackage quaremain.db
  (:use :cl)
  (:import-from :cl-dbi
                :connect-cached
                :disconnect)
  (:import-from :mito
                :connect-toplevel
                :deftable
                :table-definition
                :execute-sql
                :ensure-table-exists
                :*connection*))
(in-package :quaremain.db)


(defun db ()
  (connect-cached
   :sqlite3
   :database-name "var/quaremain.db"))

(defmacro with-connection (connection &body body)
  "Database connection wrapper."
  `(let ((*connection* ,connection))
     (unwind-protect (progn ,@body)
       (disconnect *connection*))))

(defun create-table (table-class)
  "Create new table from schema."
  (with-connection (db)
    (ensure-table-exists table-class)))
