(in-package :cl-user)
(defpackage dbi-cp-mysql-max-allowed-packet-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-mysql-max-allowed-packet-test)

(defvar *connection-pool* nil)

(setup
  (setf *connection-pool* (make-dbi-connection-pool :mysql
                                                    :database-name "test"
                                                    :username "root"
                                                    :password "password"
                                                    :host "mysql-test"
                                                    :port 3306
                                                    :initial-size 1
                                                    :max-size 2)))

(teardown
  (shutdown *connection-pool*))


(deftest test-max-allowed-packet-retrieval
  (testing "max_allowed_packet should be retrieved from MySQL"
    (let ((conn (get-connection *connection-pool*)))
      (let ((max-packet (get-max-allowed-packet conn)))
        (ok (integerp max-packet)
            "max_allowed_packet should be an integer")
        (ok (> max-packet 0)
            "max_allowed_packet should be greater than 0")
        ;; With docker-compose setting, it should be 1MB
        (ok (= max-packet 1048576)
            "max_allowed_packet should be 1MB (1048576 bytes) as configured"))
      (disconnect conn))))


(deftest test-check-packet-size-small-query
  (testing "Small query should pass packet size check"
    (let ((conn (get-connection *connection-pool*)))
      (let ((small-sql "SELECT 1")
            (params nil))
        (ok (check-packet-size conn small-sql params)
            "Small query should be within limit"))
      (disconnect conn))))


(deftest test-check-packet-size-medium-query
  (testing "Medium query should pass packet size check"
    (let ((conn (get-connection *connection-pool*)))
      (let* ((medium-string (make-string 1000 :initial-element #\a))
             (sql "SELECT ?")
             (params (list medium-string)))
        (ok (check-packet-size conn sql params)
            "Medium query should be within limit"))
      (disconnect conn))))


(deftest test-packet-size-exceeded-small-query
  (testing "Small query should not exceed packet size"
    (let ((conn (get-connection *connection-pool*)))
      (let ((small-sql "SELECT 1")
            (params nil))
        (ok (not (packet-size-exceeded-p conn small-sql params))
            "Small query should not exceed limit"))
      (disconnect conn))))


(deftest test-large-query-warning
  (testing "Large query approaching limit should trigger warning"
    (let ((conn (get-connection *connection-pool*)))
      (let* ((max-packet (get-max-allowed-packet conn)))
        
        ;; Create a string that exceeds 90% threshold
        ;; If max_allowed_packet is 1MB, create 950KB string
        (let* ((test-size (floor (* max-packet 0.95)))
               (large-string (make-string test-size :initial-element #\a))
               (warned nil))
          
          ;; Capture warning
          (handler-bind
              ((warning (lambda (w)
                          (format t "~%WARNING CAPTURED: ~A~%" w)
                          (setf warned t)
                          (muffle-warning w))))
            ;; Try to execute with large data
            ;; This should trigger a warning
            (handler-case
                (let ((query (prepare conn "SELECT ?")))
                  (execute query (list large-string)))
              (error (e)
                ;; May fail if packet is too large, that's ok
                (format t "~%Error (expected): ~A~%" e))))
          
          (ok warned
              "Warning should be triggered for large query")))
      (disconnect conn))))


(deftest test-actual-insert-with-large-data
  (testing "Insert with large data should work"
    (let ((conn (get-connection *connection-pool*)))
      ;; Create table for testing
      (do-sql conn "DROP TABLE IF EXISTS large_data_test")
      (do-sql conn "CREATE TABLE large_data_test (id INTEGER PRIMARY KEY, data TEXT)")
      
      ;; Use a fixed 10KB size for testing
      (let ((test-string (make-string 10240 :initial-element #\b)))
        
        ;; This should work without exceeding limit
        (ok (not (packet-size-exceeded-p conn "INSERT INTO large_data_test (id, data) VALUES (?, ?)"
                                         (list 1 test-string)))
            "10KB query should not exceed limit")
        
        ;; Actually insert the data
        (handler-case
            (progn
              (do-sql conn "INSERT INTO large_data_test (id, data) VALUES (?, ?)"
                      (list 1 test-string))
              
              ;; Verify it was inserted
              (let* ((query (prepare conn "SELECT LENGTH(data) as len FROM large_data_test WHERE id = 1"))
                     (result (execute query))
                     (row (fetch result)))
                (ok (= (getf row :|len|) 10240)
                    "Data length should be 10240")))
          (error (e)
            (format t "Error during insert: ~A~%" e)
            (fail "Should not raise error during insert"))))
      
      ;; Cleanup
      (do-sql conn "DROP TABLE large_data_test")
      (disconnect conn))))


(deftest test-max-allowed-packet-consistency
  (testing "max_allowed_packet should be the same for all connections"
    (let ((conn1 (get-connection *connection-pool*)))
      (let ((max-packet-1 (get-max-allowed-packet conn1)))
        (disconnect conn1)
        ;; Get another connection after returning the first one
        (let ((conn2 (get-connection *connection-pool*)))
          (let ((max-packet-2 (get-max-allowed-packet conn2)))
            (ok (= max-packet-1 max-packet-2)
                "max_allowed_packet should be the same for all connections from the same pool"))
          (disconnect conn2))))))


(deftest test-non-mysql-connection
  (testing "Non-MySQL connection should return NIL for max_allowed_packet"
    ;; Create a SQLite pool for comparison
    (let ((sqlite-pool (make-dbi-connection-pool :sqlite3
                                                  :database-name ":memory:")))
      (let ((conn (get-connection sqlite-pool)))
        (ok (null (get-max-allowed-packet conn))
            "SQLite connection should return NIL for max_allowed_packet")
        (disconnect conn))
      (shutdown sqlite-pool))))

