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
(defpackage quaremain.utilities.exception
  (:documentation "High level and concrete custom exceptions.")
  (:use :cl)
  (:export :stock-missing-property-value-error
           :row-doesnt-exist-error
           :row-with-same-name-already-exist-error
           :no-database-tables-to-be-found-error))
(in-package :quaremain.utilities.exception)

(define-condition quaremain-error (simple-error)
  ())

(define-condition stock-missing-property-value-error (quaremain-error)
  ((property-value
    :reader property-value
    :initarg :property-value
    :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "Property value of ~A is empty."
                     (property-value condition)))))

(define-condition row-doesnt-exist-error (quaremain-error)
  ((table-name
    :reader table-name
    :initarg :table-name
    :initform nil)
   (id
    :reader id
    :initarg :id
    :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "Row with unique ID ~A from TABLE ~A doesn't exist!"
                     (id condition)
                     (table-name condition)))))

(define-condition row-with-same-name-already-exist-error (quaremain-error)
  ((table-name
    :reader table-name
    :initarg :table-name
    :initform nil)
   (name
    :reader name
    :initarg :name
    :initform nil))
  (:report (lambda (condition stream)
             (format stream
                     "Row with NAME ~A from TABLE ~A already exist! No duplication allowed!"
                     (name condition)
                     (table-name condition)))))


(define-condition no-database-tables-to-be-found-error (quaremain-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No database tables to be found!"))))
