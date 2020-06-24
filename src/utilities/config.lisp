;;;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;;;; This file is part of Quaremain software which is released under
;;;; the MIT license.
;;;; For more information, see LICENSE file that is distributed along
;;;; with this software.

(in-package :cl-user)
(defpackage quaremain.utilities.config
  (:documentation "All system-wide configurations goes here.")
  (:use :cl)
  (:export :+static-directory+
           :+template-directory+
           :+seeds-directory+
           :+database-path+))
(in-package :quaremain.utilities.config)

(defparameter +static-directory+ (pathname "static/"))
(defparameter +template-directory+ (pathname "templates/"))
(defparameter +seeds-directory+ "seeds/stocks/")
(defparameter +database-path+ "var/quaremain.db")

