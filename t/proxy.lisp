(in-package :cl-user)
(defpackage dbi-cp-proxy-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-proxy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dbi-cp)' in your Lisp.


;; ----------------------------------------
;; define common test function from CL-DBI
;; ----------------------------------------

(defun test-do-sql (conn)
  ;; no exception check
  (do-sql conn "DROP TABLE IF EXISTS person")
  (do-sql conn "CREATE TABLE person (id INTEGER PRIMARY KEY, name VARCHAR(24) NOT NULL)")

  ;; returns 1
  (ok (eq (do-sql conn "INSERT INTO person (id, name) values (1, 'fukamachi')")
          1))
  (ok (eq (do-sql conn "INSERT INTO person (id, name) values (2, 'matsuyama')")
          1)))

(defun test-prepare-execute-fetch (conn)
  (let (query result)
    (setf query (prepare conn "SELECT * FROM person"))
    (setf result (execute query))
    (ok (equal (fetch-all result)
               '((:|id| 1 :|name| "fukamachi") (:|id| 2 :|name| "matsuyama"))))
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
    (ok (string= (getf (fetch result) :|name|) "snmsts"))))

(defun test-with-transaction (conn)
  (handler-case
      (progn
        (with-transaction conn
          (do-sql conn "INSERT INTO person (id, name) values (4, 'meymao')"))
        (ok (equal (fetch (execute (prepare conn "SELECT * FROM person WHERE name = 'meymao'")))
                   '(:|id| 4 :|name| "meymao"))))
    (<dbi-notsupported-error> ()
      (skip "No supported"))))

(defun test-statement-error (conn)
  (let ((c (dbi-cp.proxy::dbi-connection conn)))
    (format T "error1~%")
    (ok (signals (dbi:do-sql c "select select select")
                 '<dbi-database-error>))
    (format T "error2~%")
    (ok (signals (dbi:do-sql c "insert insert insert")
                 '<dbi-database-error>))
    (format t "start transaction")
    (dbi:begin-transaction c)
    (format t "insert~%")
    (dbi:do-sql c "INSERT INTO person (id, name) VALUES (5, 'mizuna')")
    (format T "insert ok~%")
    (format t "rollback~%")
    (dbi:rollback c)
    (format t "rollback ok~%"))
  (format t "t1~%")
;;  (with-transaction conn
  (format t "t1 transaction~%")
;;  (begin-transaction conn)
  (format t "t1 transaction set ok~%")
    ;(ok (signals (do-sql conn "INSERT")))
;;                  '<dbi-database-error>)))
  (ok (signals (execure (prepare conn "INSERT") '())))
    (format t "t1 error~%")

    (format t "t1 rollback~%")

;;  (rollback conn)
    (format t "t1 rollback done~%")
  
    (format t "t1 commit~%")
;;  (commit conn)
    (format t "t1 commit done~%")

  (format t "t2~%")
  (format t "t2~%")
  (format t "t2~%")
  (format t "t2~%")
  (format t "t2~%")
  (format t "t2~%")
 
  (format t "vvvt2 begin tran~%")
  ;;(begin-transaction conn)
  (format t "^^^t2 begin tran~%")

 
    (ok (signals (execute (prepare conn "SELECT SELECT SELECT") '())
                  '<dbi-database-error>))
    (format t "t2 error~%")
;;  (rollback conn)
    (format t "t2 rollback~%")

  (format t "t3~%")
  ;;(with-transaction conn
    (do-sql conn "INSERT INTO person (id, name) VALUES (5, 'mizuna')")
)


(defparameter *connection-pool-sqlite3* nil)
(defparameter *connection-pool-mysql* nil)
(defparameter *connection-pool-postgres* nil)

(setup
  ;; SQLite3
  (setf *connection-pool-sqlite3*
    (make-dbi-connection-pool :sqlite3
                              :database-name "/app/volumes/sqlite3-test.db"
                              :initial-size 2
                              :max-size 3))
  ;; MySQL
  (setf *connection-pool-mysql*
    (make-dbi-connection-pool :mysql
                              :database-name "test"
                              :username "root"
                              :password "password"
                              :host "mysql-test"
                              :port 3306
                              :initial-size 3
                              :max-size 4))

  ;; PostgreSQL
  (setf *connection-pool-postgres*
    (make-dbi-connection-pool :postgres
                              :database-name "test"
                              :username "dbicp"
                              :password "password"
                              :host "postgresql-test"
                              :port 5432
                              :initial-size 2
                              :max-size 2)))

(teardown
  (shutdown *connection-pool-sqlite3*)
  (shutdown *connection-pool-mysql*)
  (shutdown *connection-pool-postgres*))


(defun test-all (connection-pool)
  (let ((conn (get-connection connection-pool)))
    (unwind-protect
         (progn
           (test-do-sql conn)
           (test-prepare-execute-fetch conn)
           (test-with-transaction conn)
           (test-statement-error conn))
      (disconnect conn))))


(deftest sqlite3
  (test-all *connection-pool-sqlite3*))

(deftest mysql
  (test-all *connection-pool-mysql*))

(deftest postgres
  (test-all *connection-pool-postgres*))

