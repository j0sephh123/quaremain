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
(defpackage quaremain.web
  (:use :cl
        :caveman2
        :quaremain.config
        :quaremain.view)
  (:import-from :mito
                :deftable)
  (:import-from :quaremain.db
                :migrate-model)
  (:export :*web*))
(in-package :quaremain.web)

;;; Application.

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(deftable stock ()
  ((title :col-type (:varchar 255))
   (description :col-type :text)
   (amount :col-type (:integer 10000))))

;;; Routing rules.

;; Root pages.
(defroute "/" ()
  (migrate-model 'stock)
  (render #p"index.html"))

(defroute "/about" ()
  (render #p"about.html"))


;;; Error pages.

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (render #p"_errors/404.html"))
