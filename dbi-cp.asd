#|
  This file is a part of CL-DBI-CONNECTION-POOL project.
  Copyright (c) 2017 tamura shingo (tamura.shingo@gmail.com)
|#

#|
  connection pool for CL-DBI

  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage dbi-cp-asd
  (:use :cl :asdf))
(in-package :dbi-cp-asd)

(defsystem dbi-cp
  :version "0.1"
  :author "tamura shingo"
  :license "LLGPL"
  :depends-on (:cl-syntax
               :cl-syntax-annot
               :cl-dbi
               :bt-semaphore)
  :components ((:module "src"
                :components
                ((:file "dbi-cp" :depends-on ("connectionpool"))
                 (:file "connectionpool" :depends-on ("proxy" "error"))
                 (:file "proxy")
                 (:file "error"))))
  :description "connection pool for CL-DBI"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dbi-cp-test))))
