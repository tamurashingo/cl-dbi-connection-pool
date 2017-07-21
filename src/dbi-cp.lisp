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
                :do-sql
                :row-count
                :begin-transaction
                :commit
                :rollback)
  (:import-from :dbi-cp.error
                :<dbi-cp-no-connection>)
  (:import-from :dbi
                :execute
                :fetch
                :fetch-all
                :savepoint
                :rollback-savepoint
                :release-savepoint
                :ping
                :with-transaction

                :<dbi-error>
                :<dbi-warning>
                :<dbi-interface-error>
                :<dbi-unimplemented-error>
                :<dbi-database-error>
                :<dbi-operational-error>
                :<dbi-integrity-error>
                :<dbi-internal-error>
                :<dbi-programming-error>
                :<dbi-notsupported-error>)
  (:export :make-dbi-connection-pool
           :get-connection
           :do-sql
           :prepare
           :execute
           :fetch
           :fetch-all
           :begin-transaction
           :commit
           :rollback
           :savepoint
           :rollback-savepoint
           :release-savepoint
           :ping
           :row-count
           :with-transaction
           :disconnect
           :shutdown

           :<dbi-cp-no-connection>

           :<dbi-error>
           :<dbi-warning>
           :<dbi-interface-error>
           :<dbi-unimplemented-error>
           :<dbi-database-error>
           :<dbi-operational-error>
           :<dbi-integrity-error>
           :<dbi-internal-error>
           :<dbi-programming-error>
           :<dbi-notsupported-error>))
(in-package :dbi-cp)


(defun show-connection-pool (connection-pool)
  "for debug"
  (loop for pool across (slot-value connection-pool 'dbi-cp.connectionpool::pool)
        for counter = 0 then (1+ counter)
        do (progn
             (format T "~A:~%" counter)
             (format T "    connected :~A~%" (slot-value pool 'dbi-cp.connectionpool::connect-p))
             (format T "    available : ~A~%" (= (bt-sem:semaphore-count (slot-value pool 'dbi-cp.connectionpool::semaphore)) 0)))))

