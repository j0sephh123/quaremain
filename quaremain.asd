#-sbcl (error "This software only runnable on SBCL implementation.")
(defsystem "quaremain"
  :version "0.8.6"
  :author "Momozor"
  :license "MIT"
  :depends-on ("clack"
               "lack"
               "caveman2"
               "cl-ppcre"
               "uiop"

               ;; HTML template rendering.
               "djula"

               ;; For database.
               "sxql"
               "datafly"
               "dbd-sqlite3"

               "cl-json"

               ;; Logging.
               "log4cl"

               ;; Deploying
               "lack-middleware-static"
               "lack-middleware-session"
               "hunchentoot"
               "clack-handler-hunchentoot")
  :components ((:module "src/utilities"
                        :components
                        ((:file "string")
                         (:file "config")
                         (:file "database" :depends-on ("config" "exception"))
                         (:file "exception")))
               
               (:module "src"
                        :depends-on ("src/models/stock" "src/utilities")
                        :components
                        ((:file "main" :depends-on ("view"
                                                    "web"))
                         (:file "web" :depends-on ("view"))
                         (:file "view")))
               
               (:module "src/models/stock"
                        :depends-on ("src/utilities")
                        :components
                        ((:file "stock" :depends-on ("food"
                                                     "water"
                                                     "medicine"
                                                     "weapon"))
                         (:file "food")
                         (:file "water")
                         (:file "medicine")
                         (:file "weapon")
                         (:file "survival" :depends-on ("stock")))))
  :description "Manage your basic survival resources like food and water
for preparation of emergency times"

  ;; include cl-webkit2 if
  ;; bundling webkit2gtk libraries are needed
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "quaremain"
  :entry-point "quaremain:main"
  :in-order-to ((test-op (test-op "quaremain/tests/unit")
                         (test-op "quaremain/tests/functional"))))

(defsystem "quaremain/tests/unit"
  :author "Momozor"
  :license "MIT"
  :depends-on ("quaremain"
               "rove")
  :components ((:module "tests/unit/utilities"
                        :components
                        ((:file "string")))
               (:module "tests/unit/models/stock"
                        :components
                        ((:file "stock")
                         (:file "survival"))))
  :description "Unit test system for quaremain"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "quaremain/tests/functional"
  :author "Momozor"
  :license "MIT"
  :depends-on ("quaremain"
               "rove"
               "dexador")
  :components ((:module "tests/functional/utilities"
                        :components
                        ((:file "database")))
               (:module "tests/functional/models/stock"
                        :components
                        ((:file "food")
                         (:file "water")
                         (:file "medicine")
                         (:file "weapon")
                         (:file "stock")
                         (:file "survival")))
               (:module "tests/functional"
                        :components
                        ((:file "web"))))
  :description "Functional test system for quaremain"
  :perform (test-op (op c) (symbol-call :rove :run c)))
