(in-package :cl-user)
(defpackage quaremain.db
  (:use :cl)
  (:import-from :cl-dbi
                :connect-cached
                :disconnect)
  (:import-from :datafly
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
