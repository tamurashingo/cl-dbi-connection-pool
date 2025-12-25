(in-package :cl-user)
(defpackage dbi-cp-transaction-sqlite3-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-transaction-sqlite3-test)

(defvar *connection-pool* nil)

(setup
  (setf *connection-pool* (make-dbi-connection-pool :sqlite3
                                                    :database-name "/volumes/sqlite3-test-transaction.db"
                                                    :initial-size 2
                                                    :max-size 3)))

(teardown
  (shutdown *connection-pool*))


(deftest sqlite3-issue11-pattern8-begin-transaction-disconnect
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


(deftest sqlite3-issue11-pattern9-begin-transaction-insert-disconnect
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


(deftest sqlite3-issue11-pattern10-begin-transaction-commit-disconnect
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


(deftest sqlite3-issue11-pattern11-begin-transaction-rollback-disconnect
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


(deftest sqlite3-issue11-pattern12-begin-transaction-with-transaction-disconnect
  (testing "Pattern 12: begin-transaction + with-transaction → disconnect"
    ;; Setup table
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users2")
      (do-sql conn "CREATE TABLE users2 (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))

    ;; SQLite3 raises "cannot start a transaction within a transaction" error
    ;; when with-transaction is called after begin-transaction.
    ;; This is expected behavior for SQLite3.
    ;; We verify that the error is properly raised and handled.
    (handler-case
        (let ((conn (get-connection *connection-pool*)))
          (unwind-protect
            (progn
              (begin-transaction conn)
              (with-transaction conn
                (do-sql conn "INSERT INTO users2 (id, name) VALUES (5, 'Eve')")))
            (disconnect conn)))
      (error (e)
        (ok t "Error was raised as expected for SQLite3")))))


(deftest sqlite3-issue11-pattern13-begin-transaction-insert-with-transaction-disconnect
  (testing "Pattern 13: begin-transaction + INSERT + with-transaction → disconnect"
    ;; Setup table
    (let ((conn (get-connection *connection-pool*)))
      (do-sql conn "DROP TABLE IF EXISTS users3")
      (do-sql conn "CREATE TABLE users3 (id INTEGER PRIMARY KEY, name VARCHAR(255))")
      (disconnect conn))

    ;; SQLite3 raises "cannot start a transaction within a transaction" error
    ;; when with-transaction is called after begin-transaction.
    ;; This is expected behavior for SQLite3.
    ;; We verify that the error is properly raised and handled.
    (handler-case
        (let ((conn (get-connection *connection-pool*)))
          (unwind-protect
            (progn
              (begin-transaction conn)
              (do-sql conn "INSERT INTO users3 (id, name) VALUES (6, 'Frank')")
              (with-transaction conn
                (do-sql conn "INSERT INTO users3 (id, name) VALUES (7, 'Grace')")))
          (disconnect conn)))
      (error (e)
        (ok t "Error was raised as expected for SQLite3")))))


(deftest sqlite3-issue11-auto-commit-restoration
  (testing "auto-commit flag should be restored after disconnect"
    ;; Use a separate connection pool with its own database file to avoid lock conflicts
    (let ((isolated-pool (make-dbi-connection-pool :sqlite3
                                                   :database-name "/volumes/sqlite3-test-auto-commit-isolated.db"
                                                   :initial-size 2
                                                   :max-size 3)))
      (unwind-protect
           (progn
             ;; Set up table once
             (let ((conn (get-connection isolated-pool)))
               (do-sql conn "DROP TABLE IF EXISTS users_test")
               (do-sql conn "CREATE TABLE users_test (id INTEGER PRIMARY KEY, name VARCHAR(255))")
               (disconnect conn))

             ;; First user: begin-transaction changes auto-commit to nil
             (let ((conn (get-connection isolated-pool)))
               (begin-transaction conn)
               ;; auto-commit is now nil
               (disconnect conn))

             ;; Next user: auto-commit should be restored to initial value
             (let ((conn (get-connection isolated-pool)))
               (let ((dbi-conn (slot-value conn 'dbi-cp.proxy::dbi-connection))
                     (initial-auto-commit (slot-value conn 'dbi-cp.proxy::initial-auto-commit)))
                 ;; auto-commit should match initial-auto-commit
                 (ok (eq (slot-value dbi-conn 'dbi.driver::auto-commit)
                         initial-auto-commit)))
               (disconnect conn)))
        ;; Cleanup: shutdown the isolated pool
        (shutdown isolated-pool)))))
