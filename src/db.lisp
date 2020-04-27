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
                :*connection*)
  (:export :db
           :with-connection
           :with-connection-execute))
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

(defmacro with-connection-execute (&body body)
  `(with-connection (db)
     (datafly:execute ,@body)))
