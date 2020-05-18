(in-package :cl-user)
(defpackage dbi-cp-proxy-test
  (:use :cl
        :dbi-cp
        :prove))
(in-package :dbi-cp-proxy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dbi-cp)' in your Lisp.


;; ----------------------------------------
;; define common test function from CL-DBI
;; ----------------------------------------

(defun test-do-sql (conn)
  (is (do-sql conn "DROP TABLE IF EXISTS person") 0)
  (is (do-sql conn "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL)")
      0)
  (is (do-sql conn "INSERT INTO person (id, name) values (1, 'fukamachi')")
      1)
  (is (do-sql conn "INSERT INTO person (id, name) values (2, 'matsuyama')")
      1))

(defun test-prepare-execute-fetch (conn)
  (let (query result)
    (setf query (prepare conn "SELECT * FROM person"))
    (setf result (execute query))
    (is (fetch-all result)
        '((:|id| 1 :|name| "fukamachi") (:|id| 2 :|name| "matsuyama")))
    (setf result (execute query))
    (let ((result (fetch result)))
      (is (getf result :|name|) "fukamachi"))
    (let ((result (fetch result)))
      (is (getf result :|name|) "matsuyama"))
    (is (fetch result) nil))

;; in MySQL and PostgreSQL, query parameter NIL occurrs an error
;;  (let* ((query (prepare conn "SELECT * FROM person WHERE name = ?"))
;;         (result (execute query nil)))
;;    (is (fetch result) nil))

  (execute (prepare conn "INSERT INTO person (id, name) VALUES (3, 'snmsts')"))

  (is (row-count conn) 1)

  (let* ((query (prepare conn "SELECT * FROM person WHERE name = ?"))
         (result (execute query '("snmsts"))))
    (is (getf (fetch result) :|name|) "snmsts")))

(defun test-with-transaction (conn)
  (handler-case
      (progn
        (with-transaction conn
          (do-sql conn "INSERT INTO person (id, name) values (4, 'meymao')"))
        (is (fetch (execute (prepare conn "SELECT * FROM person WHERE name = 'meymao'")))
            '(:|id| 4 :|name| "meymao")))
    (<dbi-notsupported-error> ()
      (skip 1 "No supported"))))

(defun test-statement-error (conn)
  (is-type (handler-case (do-sql conn "INSERT")
             (error (e) e))
           '<dbi-database-error>)
  (rollback conn) ;; for PostgreSQL
  (is-type (handler-case (execute (prepare conn "SELECT SELECT SELECT"))
             (error (e) e))
           '<dbi-database-error>)
  (rollback conn) ;; for PostgreSQL
  (do-sql conn "INSERT INTO person (id, name) VALUES (5, 'mizuna')"))



(plan nil)

;; SQLite3
(defparameter *connection-pool-sqlite3*
  (make-dbi-connection-pool :sqlite3
                            :database-name ":memory:"
                            :initial-size 2
                            :max-size 3))


;; MySQL
(defparameter *connection-pool-mysql*
  (make-dbi-connection-pool :mysql
                            :database-name "dbi-cp"
                            :username "nobody"
                            :password "nobody"
                            :initial-size 3
                            :max-size 4))

;; PostgreSQL
(defparameter *connection-pool-postgres*
  (make-dbi-connection-pool :postgres
                            :database-name "dbi-cp"
                            :username "nobody"
                            :password "nobody"
                            :initial-size 2
                            :max-size 2))


(defun test-all (connection-pool)
  (let ((conn (get-connection connection-pool)))
    (unwind-protect
         (progn
           (test-do-sql conn)
           (test-prepare-execute-fetch conn)
           (test-with-transaction conn)
           (test-statement-error conn))
      (disconnect conn))))

(test-all *connection-pool-sqlite3*)
(test-all *connection-pool-mysql*)
(test-all *connection-pool-postgres*)

(shutdown *connection-pool-sqlite3*)
(shutdown *connection-pool-mysql*)
(shutdown *connection-pool-postgres*)


(finalize)
