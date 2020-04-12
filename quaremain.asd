(defsystem "quaremain"
    :version "0.1.0"
    :author "Momozor"
    :license "Apache-2.0"
    :depends-on ("clack"
                 "lack"
                 "caveman2"
                 "envy"
                 "cl-ppcre"
                 "uiop"

                 ;; HTML Template.
                 "djula"

                 ;; For DB.
                 "sxql"
                 "datafly"

                 ;; Logging.
                 "log4cl"

                 ;; Deploying.
                 "cl-webkit2" ;; foreign
                 "sqlite"  ;; foreign
                 "deploy")
    :components ((:module "src"
                          :components
                          ((:file "main" :depends-on ("config" "view" "db"))
                           (:file "web" :depends-on ("view"))
                           (:file "view" :depends-on ("config"))
                           (:file "db" :depends-on ("config"))
                           (:file "config"))))
    :description "Manage your basic survival stocking needs, for the future."
    :defsystem-depends-on (:deploy)    
    :build-operation "deploy-op"
    :build-pathname "quaremain"
    :entry-point "quaremain:main"
    :in-order-to ((test-op (test-op "quaremain/tests"))))

(defsystem "quaremain/tests"
  :author "Momozor"
  :license "Apache-2.0"
  :depends-on ("quaremain"
               "rove")
  :components ((:module "tests"
                        :components
                        ((:file "main"))))
  :description "Test system for quaremain"
  :perform (test-op (op c) (symbol-call :rove :run c)))
