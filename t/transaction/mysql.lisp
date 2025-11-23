(in-package :cl-user)
(defpackage dbi-cp-transaction-mysql-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-transaction-mysql-test)

(defvar *connection-pool* nil)

(setup
  (setf *connection-pool* (make-dbi-connection-pool :mysql
                                                    :database-name "test"
                                                    :username "root"
                                                    :password "password"
                                                    :host "mysql-test"
                                                    :port 3306
                                                    :initial-size 2
                                                    :max-size 3)))

(teardown
  (shutdown *connection-pool*))


(deftest mysql-issue11-pattern8-begin-transaction-disconnect
  (testing "Pattern 8: begin-transaction → disconnect"
    ;; First user: begin-transaction but no commit/rollback
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users")
      (do-sql conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (begin-transaction conn)
      (do-sql conn "INSERT INTO users (id, name) VALUES (1, 'Alice')")
      (disconnect conn))
    
    ;; Next user: Should get clean connection
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query (prepare conn "SELECT * FROM users WHERE id = 1"))
             (result (execute query)))
        ;; Alice should not exist (rolled back)
        (ok (null (fetch result))))
      (disconnect conn))))


(deftest mysql-issue11-pattern9-begin-transaction-insert-disconnect
  (testing "Pattern 9: begin-transaction + INSERT → disconnect"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users")
      (do-sql conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction + INSERT but no commit
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      (do-sql conn "INSERT INTO users (id, name) VALUES (2, 'Bob')")
      (disconnect conn))
    
    ;; Next user: Should get clean connection
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query (prepare conn "SELECT * FROM users WHERE id = 2"))
             (result (execute query)))
        ;; Bob should not exist (rolled back)
        (ok (null (fetch result))))
      (disconnect conn))))


(deftest mysql-issue11-pattern10-begin-transaction-commit-disconnect
  (testing "Pattern 10: begin-transaction + commit → disconnect (commit does not work)"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users")
      (do-sql conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction + INSERT + commit (but commit does nothing)
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      (do-sql conn "INSERT INTO users (id, name) VALUES (3, 'Charlie')")
      (commit conn)  ;; This does nothing because *transaction-state* is nil
      (disconnect conn))
    
    ;; Next user: Should get clean connection
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query (prepare conn "SELECT * FROM users WHERE id = 3"))
             (result (execute query)))
        ;; Charlie should not exist (rolled back on disconnect)
        (ok (null (fetch result))))
      (disconnect conn))))


(deftest mysql-issue11-pattern11-begin-transaction-rollback-disconnect
  (testing "Pattern 11: begin-transaction + rollback → disconnect (rollback does not work)"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users")
      (do-sql conn "CREATE TABLE users (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction + INSERT + rollback (but rollback does nothing)
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      (do-sql conn "INSERT INTO users (id, name) VALUES (4, 'Dave')")
      (rollback conn)  ;; This does nothing because *transaction-state* is nil
      (disconnect conn))
    
    ;; Next user: Should get clean connection
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query (prepare conn "SELECT * FROM users WHERE id = 4"))
             (result (execute query)))
        ;; Dave should not exist (rolled back on disconnect)
        (ok (null (fetch result))))
      (disconnect conn))))


(deftest mysql-issue11-pattern12-begin-transaction-with-transaction-disconnect
  (testing "Pattern 12: begin-transaction + with-transaction → disconnect"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users2")
      (do-sql conn "CREATE TABLE users2 (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction + with-transaction
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      (with-transaction conn
        (do-sql conn "INSERT INTO users2 (id, name) VALUES (5, 'Eve')"))
      (disconnect conn))
    
    ;; Next user: Should see committed data
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query (prepare conn "SELECT * FROM users2 WHERE id = 5"))
             (result (execute query)))
        ;; Eve should exist (committed by with-transaction)
        (ok (equal (fetch result) '(:|id| 5 :|name| "Eve"))))
      (disconnect conn))))


(deftest mysql-issue11-pattern13-begin-transaction-insert-with-transaction-disconnect
  (testing "Pattern 13: begin-transaction + INSERT + with-transaction → disconnect"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users3")
      (do-sql conn "CREATE TABLE users3 (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction + INSERT + with-transaction
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      (do-sql conn "INSERT INTO users3 (id, name) VALUES (6, 'Frank')")
      (with-transaction conn
        (do-sql conn "INSERT INTO users3 (id, name) VALUES (7, 'Grace')"))
      (disconnect conn))
    
    ;; Next user: Both should exist (committed by with-transaction)
    (let ((conn (get-connection *connection-pool*)))
      (let* ((query1 (prepare conn "SELECT * FROM users3 WHERE id = 6"))
             (result1 (execute query1))
             (query2 (prepare conn "SELECT * FROM users3 WHERE id = 7"))
             (result2 (execute query2)))
        ;; Frank should exist
        (ok (equal (fetch result1) '(:|id| 6 :|name| "Frank")))
        ;; Grace should exist
        (ok (equal (fetch result2) '(:|id| 7 :|name| "Grace"))))
      (disconnect conn))))


(deftest mysql-issue11-auto-commit-restoration
  (testing "auto-commit flag should be restored after disconnect"
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users4")
      (do-sql conn "CREATE TABLE users4 (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))
    
    ;; First user: begin-transaction changes auto-commit to nil
    (let ((conn (get-connection *connection-pool*)))
      (begin-transaction conn)
      ;; auto-commit is now nil
      (disconnect conn))
    
    ;; Next user: auto-commit should be restored to initial value
    (let ((conn (get-connection *connection-pool*)))
      (let ((dbi-conn (slot-value conn 'dbi-cp.proxy::dbi-connection))
            (initial-auto-commit (slot-value conn 'dbi-cp.proxy::initial-auto-commit)))
        ;; auto-commit should match initial-auto-commit
        (ok (eq (slot-value dbi-conn 'dbi.driver::auto-commit)
                initial-auto-commit)))
      (disconnect conn))))
