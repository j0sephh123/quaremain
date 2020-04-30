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
(defpackage quaremain.models.stock.stock
  (:documentation "Stock abstract model related procedures.")
  (:use :cl)
  (:import-from :quaremain.utilities.string
                :string-to-keyword)
  (:import-from :quaremain.utilities.database
                :insert-datum-into-table
                :get-all-datum-from-table)
  (:export :create-new-stock))
(in-package :quaremain.models.stock.stock)


(defun create-new-stock (stock-category name description
                         amount cost-per-package calories-per-package)
  (if (string-equal stock-category "food")
      (insert-datum-into-table (string-to-keyword stock-category)
        :name name
        :description description
        :amount amount
        :cost-per-package cost-per-package
        :calories-per-package calories-per-package)

      (insert-datum-into-table (string-to-keyword stock-category)
        :name name
        :description description
        :amount amount
        :cost-per-package cost-per-package)))
