(in-package :cl-user)
(defpackage dbi-cp
  (:use :cl)
  (:nicknames :cl-dbi-connection-pool)
  (:import-from :dbi-cp.connectionpool
                :make-connection-pool
                :disconnect-all
                :get-connection
                :disconnect)
  (:export :make-connection-pool
           :disconnect-all
           :get-connection
           :disconnect))
(in-package :dbi-cp)


