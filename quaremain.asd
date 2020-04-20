(defsystem "quaremain"
    :version "0.4.5"
    :author "Momozor"
    :license "GPL-3.0-or-later"
    :depends-on ("clack"
                 "lack"
                 "caveman2"
                 "cl-ppcre"
                 "uiop"

                 ;; HTML Template.
                 "djula"

                 ;; For DB.
                 "sxql"
                 "datafly"
                 "dbd-sqlite3"

                 ;; Logging.
                 "log4cl"

                 ;; Deploying
                 "deploy"
                 "lack-middleware-static"
                 "lack-middleware-session"
                 "hunchentoot"
                 "clack-handler-hunchentoot"
                 ;; "cl-webkit2" if bundling webkit2 and gtk stuff are needed
                 )
    :components ((:module "src"
                          :components
                          ((:file "main" :depends-on ("config" "view" "db" "web"))
                           (:file "web" :depends-on ("view"))
                           (:file "view" :depends-on ("config"))
                           (:file "db" :depends-on ("config"))
                           (:file "config"))))
    :description "A software to manage resources for emergency times."
    

    :defsystem-depends-on (:deploy)
    :build-operation "deploy-op"
    :build-pathname "quaremain"
    :entry-point "quaremain:main"
    :in-order-to ((test-op (test-op "quaremain/tests"))))

(defsystem "quaremain/tests"
    :author "Momozor"
    :license "GPL-3.0-or-later"
    :depends-on ("quaremain"
                 "rove")
    :components ((:module "tests"
                          :components
                          ((:file "main"))))
    :description "Test system for quaremain"
    :perform (test-op (op c) (symbol-call :rove :run c)))
