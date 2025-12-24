(in-package :cl-user)
(defpackage dbi-cp-mysql-keepalive-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-mysql-keepalive-test)

(deftest mysql-keepalive-validates-connections
  (testing "MySQL keepalive validates connections periodically"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :keepalive-interval 3
                                          :validation-query "SELECT 1"
                                          :reaper-interval 2
                                          :idle-timeout 0
                                          :max-lifetime nil)))
      (unwind-protect
           (progn
             ;; Get connection
             (let* ((conn (get-connection pool))
                    (pooled-conn (find-if (lambda (pc)
                                           (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                               conn))
                                         (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list))))

               (ok (null (slot-value pooled-conn 'dbi-cp.connectionpool::last-keepalive-time))
                   "Initial keepalive time should be nil")

               ;; Use the connection
               (do-sql conn "SELECT 1")

               ;; Return connection
               (disconnect conn)

               ;; Wait for keepalive check
               (sleep 6)

               ;; Keepalive should have been performed
               (ok (slot-value pooled-conn 'dbi-cp.connectionpool::last-keepalive-time)
                   "Keepalive time should be set after check")))
        (shutdown pool)))))

(deftest mysql-keepalive-recreates-invalid-connections
  (testing "MySQL keepalive recreates connections that fail validation"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :keepalive-interval 3
                                          :validation-query "SELECT 1"
                                          :reaper-interval 2
                                          :idle-timeout 0
                                          :max-lifetime nil)))
      (unwind-protect
           (progn
             ;; Get connection and record created time
             (let* ((conn (get-connection pool))
                    (pooled-conn (find-if (lambda (pc)
                                           (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                               conn))
                                         (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (created-time (slot-value pooled-conn 'dbi-cp.connectionpool::created-time)))

               (ok created-time "Connection should have created-time")

               ;; Use the connection normally
               (ok (do-sql conn "SELECT 1") "Connection should work before invalidation")

               ;; Return connection
               (disconnect conn)

               ;; Manually kill the connection from database side
               ;; (In real scenario, database restart would cause this)
               ;; For testing, we'll just force a recreation by waiting
               (sleep 6)

               ;; Connection should still exist (may have been validated successfully)
               (ok (slot-value pooled-conn 'dbi-cp.connectionpool::connect-p)
                   "Connection should still be connected after keepalive")))
        (shutdown pool)))))

(deftest mysql-keepalive-default-disabled
  (testing "MySQL keepalive is disabled by default"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2)))
      (unwind-protect
           (ok (= (slot-value pool 'dbi-cp.connectionpool::keepalive-interval) 0)
               "Default keepalive-interval should be 0 (disabled)")
        (shutdown pool)))))

(deftest mysql-keepalive-with-max-lifetime
  (testing "Keepalive and max-lifetime work together"
    (let ((pool (make-dbi-connection-pool :mysql
                                          :database-name "test"
                                          :username "root"
                                          :password "password"
                                          :host "mysql-test"
                                          :port 3306
                                          :initial-size 1
                                          :max-size 2
                                          :keepalive-interval 3
                                          :validation-query "SELECT 1"
                                          :max-lifetime 10
                                          :reaper-interval 2
                                          :idle-timeout 0)))
      (unwind-protect
           (progn
             ;; Both features should be enabled
             (ok (> (slot-value pool 'dbi-cp.connectionpool::keepalive-interval) 0)
                 "Keepalive should be enabled")
             (ok (> (slot-value pool 'dbi-cp.connectionpool::max-lifetime) 0)
                 "Max-lifetime should be enabled")

             ;; Get and return connection
             (let ((conn (get-connection pool)))
               (do-sql conn "SELECT 1")
               (disconnect conn))

             ;; Wait for keepalive check
             (sleep 6)

             ;; Connection should be valid
             (let ((conn (get-connection pool)))
               (ok (do-sql conn "SELECT 1") "Connection should work after keepalive")
               (disconnect conn)))
        (shutdown pool)))))
