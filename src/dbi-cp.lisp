(in-package :cl-user)
(defpackage dbi-cp
  (:use :cl)
  (:nicknames :cl-dbi-connection-pool)
  (:import-from :dbi-cp.connectionpool
                :make-dbi-connection-pool
                :shutdown
                :get-connection)
  (:import-from :dbi-cp.proxy
                :disconnect
                :prepare
                :do-sql)
  (:export :make-dbi-connection-pool
           :get-connection
           :do-sql
           :prepare
           :disconnect
           :shutdown))
(in-package :dbi-cp)
