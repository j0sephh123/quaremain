(defsystem "quaremain"
  :version "0.1.0"
  :author "Momozor"
  :license "gpl-3.0-or-later"
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; HTML Template.
               "djula"

               ;; For DB.
               "datafly"
               "sxql"

               ;; Logging.
               "log4cl"

               ;; Email access.
               "cl-smtp")
  :components ((:module "src"
                        :components
                        ((:file "main" :depends-on ("config" "view" "db"))
                         (:file "web" :depends-on ("view"
                                                   "models/user-model"))
                         (:file "models/user-model" :depends-on ("db"))
                         (:file "view" :depends-on ("config"))
                         (:file "db" :depends-on ("config"))
                         (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "quaremain/tests"))))

(defsystem "quaremain/tests"
  :author "Momozor"
  :license "gpl-3.0-or-later"
  :depends-on ("quaremain"
               "rove")
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "Test system for quaremain"
  :perform (test-op (op c) (symbol-call :rove :run c)))
