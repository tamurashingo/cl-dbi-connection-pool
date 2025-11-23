#|
  This file is a part of CL-DBI-CONNECTION-POOL project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-cp-test-asd
  (:use :cl :asdf))
(in-package :dbi-cp-test-asd)

(defsystem dbi-cp-test
  :author "tamura shingo"
  :license "LLGPL"
  :depends-on (:dbi-cp
               :rove)
  :components ((:module "t"
                :components
                ((:file "dbi-cp")
                 (:module "proxy"
                  :components
                  ((:file "sqlite3")
                   (:file "mysql")
                   (:file "postgres")))
                 (:module "transaction"
                  :components
                  ((:file "sqlite3")
                   (:file "mysql")
                   (:file "postgres"))))))
  :description "Test system for CL-DBI-CONNECTION-POOL"

  :perform (test-op (op c)
             (uiop:symbol-call :rove :run c)))
