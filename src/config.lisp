;; Copyright (C) 2020 Momozor

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
(defpackage quaremain.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :absolute-path
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :quaremain.config)

(setf (config-env-var) "APP_ENV")

(defun absolute-path (pathname-string)
  (uiop:unix-namestring
   (uiop:merge-pathnames*
    (uiop:parse-unix-namestring pathname-string))))

(defparameter *static-directory* (pathname (absolute-path "static/")))
(defparameter *template-directory* (pathname (absolute-path "templates/")))

(defconfig |development|
    '(:debug t))

(defconfig |test|
    '(:debug t))

(defconfig |production|
    '(:debug nil))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
