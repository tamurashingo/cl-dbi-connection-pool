(in-package :cl-user)
(defpackage dbi-cp-mysql-max-lifetime-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-mysql-max-lifetime-test)

(deftest mysql-retrieves-wait-timeout
  (testing "MySQL connection pool retrieves wait_timeout from server"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :max-lifetime 1800)))
      (unwind-protect
           (progn
             ;; Get wait_timeout from server directly
             (let* ((conn (get-connection pool))
                    (query (prepare conn "SHOW VARIABLES LIKE 'wait_timeout'"))
                    (result (execute query))
                    (row (fetch result))
                    (wait-timeout (parse-integer (getf row :|Value|))))
               (ok (> wait-timeout 0) "wait_timeout should be retrieved")

               ;; If max-lifetime was adjusted, it should be less than wait_timeout
               (let ((actual-max-lifetime (slot-value pool 'dbi-cp.connectionpool::max-lifetime)))
                 (ok (< actual-max-lifetime wait-timeout)
                     "max-lifetime should be less than wait_timeout"))

               (disconnect conn)))
        (shutdown pool)))))

(deftest mysql-warns-when-max-lifetime-exceeds-wait-timeout
  (testing "Warns when max-lifetime exceeds MySQL wait_timeout"
    (let* ((warnings nil)
           (*error-output* (make-string-output-stream)))
      ;; Set a very large max-lifetime that will exceed wait_timeout
      (let ((pool (make-dbi-connection-pool :mysql
                                            :database-name "test"
                                            :username "root"
                                            :password "password"
                                            :host "mysql-test"
                                            :port 3306
                                            :initial-size 1
                                            :max-size 2
                                            :max-lifetime 99999)))
        (unwind-protect
             (progn
               (let ((warning-output (get-output-stream-string *error-output*)))
                 ;; A warning should have been issued
                 (ok (or (search "max-lifetime" warning-output)
                         (search "wait_timeout" warning-output)
                         ;; The max-lifetime should have been adjusted
                         (< (slot-value pool 'dbi-cp.connectionpool::max-lifetime) 99999))
                     "Should warn or adjust max-lifetime when it exceeds wait_timeout")))
          (shutdown pool))))))

(deftest mysql-max-lifetime-recreates-connections
  (testing "MySQL connections are recreated after max-lifetime"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :max-lifetime 3
                                          :idle-timeout 0
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Get connection and its created time
             (let* ((conn (get-connection pool))
                    (pooled-conn (find-if (lambda (pc)
                                           (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                               conn))
                                         (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (created-time (slot-value pooled-conn 'dbi-cp.connectionpool::created-time)))

               (ok created-time "Connection should have created-time")

               ;; Use the connection
               (do-sql conn "SELECT 1")

               ;; Return connection
               (disconnect conn)

               ;; Wait for max-lifetime to expire
               (sleep 4)

               ;; Get connection again - it should be recreated
               (let ((new-conn (get-connection pool)))
                 (let* ((new-pooled-conn (find-if (lambda (pc)
                                                   (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                       new-conn))
                                                 (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                        (new-created-time (slot-value new-pooled-conn 'dbi-cp.connectionpool::created-time)))
                   (ok (> new-created-time created-time)
                       "Connection should be recreated with newer created-time")

                   ;; New connection should work
                   (ok (do-sql new-conn "SELECT 1") "Recreated connection should work")

                   (disconnect new-conn)))))
        (shutdown pool)))))

(deftest mysql-max-lifetime-with-default-wait-timeout
  (testing "max-lifetime works with MySQL default wait_timeout"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :max-lifetime 1800)))
      (unwind-protect
           (progn
             ;; Check that max-lifetime was set appropriately
             (let* ((actual-max-lifetime (slot-value pool 'dbi-cp.connectionpool::max-lifetime))
                    (conn (get-connection pool))
                    (query (prepare conn "SHOW VARIABLES LIKE 'wait_timeout'"))
                    (result (execute query))
                    (row (fetch result))
                    (wait-timeout (parse-integer (getf row :|Value|))))

               (ok (<= actual-max-lifetime (- wait-timeout 60))
                   (format nil "max-lifetime (~A) should be at most wait_timeout - 60 (~A)"
                          actual-max-lifetime (- wait-timeout 60)))

               (disconnect conn)))
        (shutdown pool)))))
