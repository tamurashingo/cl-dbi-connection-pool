(in-package :cl-user)
(defpackage cl-dbi-connection-pool-asd
  (:use :cl :asdf))
(in-package :cl-dbi-connection-pool-asd)

(defsystem cl-dbi-connection-pool
  :version "0.1"
  :author "tamura shingo"
  :license "LLGPL"
  :depends-on (:dbi-cp))
