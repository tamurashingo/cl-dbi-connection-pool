(in-package :cl-user)
(defpackage dbi-cp-proxy-postgres-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-proxy-postgres-test)

(defvar *connection-pool* nil)

(setup
  (setf *connection-pool* (make-dbi-connection-pool :postgres
                                                    :database-name "test"
                                                    :username "dbicp"
                                                    :password "password"
                                                    :host "postgresql-test"
                                                    :port 5432
                                                    :initial-size 2
                                                    :max-size 2)))


(teardown
  (shutdown *connection-pool*))


(deftest postgres-do-sql
  (let ((conn (get-connection *connection-pool*)))
    (with-transaction conn
      (do-sql conn "DROP TABLE IF EXISTS person")
      (do-sql conn "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(255) NOT NULL)"))

    (ok (eq (do-sql conn "INSERT INTO person (id, name) values (1, 'fukamachi')")
            1))
    (ok (eq (do-sql conn "INSERT INTO person (id, name) values (2, 'matsuyama')")
            1))
    (disconnect conn)))


(deftest postgres-prepare-execute-fetch
  (let ((conn (get-connection *connection-pool*)))

    (let (query result)
      (setf query (prepare conn "SELECT * FROM person"))
      (setf result (execute query))
      (ok (equal (fetch-all result)
                 '((:|id| 1 :|name| "fukamachi")
                   (:|id| 2 :|name| "matsuyama"))))

      (setf result (execute query))
      (let ((result (fetch result)))
        (ok (string= (getf result :|name|) "fukamachi")))
      (let ((result (fetch result)))
        (ok (string= (getf result :|name|) "matsuyama")))
      (ok (null (fetch result))))

    (let* ((query (prepare conn "SELECT * FROM person WHERE name = ?"))
           (result (execute query (list ""))))
      (ok (null (fetch result))))

    (execute (prepare conn "INSERT INTO person (id, name) VALUES (3, 'snmsts')"))

    (ok (eq (row-count conn) 1))

    (let* ((query (prepare conn "SELECT * FROM person WHERE name = ?"))
           (result (execute query (list "snmsts"))))
      (ok (string= (getf (fetch result) :|name|) "snmsts")))

    (disconnect conn)))

(deftest postgres-with-transaction-commit
  (let ((conn (get-connection *connection-pool*)))
    (handler-case
        (progn
          (with-transaction conn
            (do-sql conn "INSERT INTO person (id, name) values (4, 'meymao')"))
          (ok (equal (fetch (execute (prepare conn "SELECT * FROM person WHERE name = 'meymao'")))
                     '(:|id| 4 :|name| "meymao"))))
      (<dbi-notsupported-error> ()
        (skip "No supported")))
    (disconnect conn)))

(deftest postgres-with-transaction-rollback
  (let ((conn (get-connection *connection-pool*)))
    (handler-case
        (progn
          (with-transaction conn
            (do-sql conn "INSERT INTO person (id, name) values (5, 'mizuna')")
            (rollback conn))
          (ok (null (fetch (execute (prepare conn "SELECT * FROM person WHERE name = 'mizuna'"))))))
      (<dbi-notsupported-error> ()
        (skip "No supported")))
    (disconnect conn)))


(deftest postgres-statement-error
  (let ((conn (get-connection *connection-pool*)))
    ;;
    ;; In PostgreSQL, If an error occurs during a transaction,
    ;; the entire transaction becomes invalid until a Commit/Rollback is performed
    ;;
    (with-transaction conn
      (ok (signals (do-sql conn "INSERT")
              '<dbi-database-error>))
      (rollback conn))
                             

    (with-transaction conn
      (ok (signals (execute (prepare conn "SELECT SELECT SELECT") '())
              '<dbi-database-error>))
      (rollback conn))


    (with-transaction conn
      (do-sql conn "INSERT INTO person (id, name) VALUES (5, 'mizuna')"))

    (disconnect conn)))
