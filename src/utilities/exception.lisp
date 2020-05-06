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
  (:export :stock-missing-property-value-error))
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
