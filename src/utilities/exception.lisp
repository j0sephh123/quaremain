;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;;
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.utilities.exception
  (:documentation "High level and concrete custom exceptions.")
  (:use :cl)
  (:export :stock-missing-property-value-error
           :row-doesnt-exist-error
           :row-with-same-name-already-exist-error
           :no-database-tables-to-be-found-error
           :total-required-survival-resources-is-too-low-error
           :all-stocks-empty-error))
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

(define-condition total-required-survival-resources-is-too-low-error (quaremain-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Total required survival resources is too low! Consider stocking more food and water!"))))

(define-condition all-stocks-empty-error (quaremain-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "All stocks are empty!"))))
