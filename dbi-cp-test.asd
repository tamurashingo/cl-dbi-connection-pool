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
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "dbi-cp"))))
  :description "Test system for CL-DBI-CONNECTION-POOL"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
