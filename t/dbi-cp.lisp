(in-package :cl-user)
(defpackage dbi-cp-test
  (:use :cl
        :dbi-cp
        :rove))
(in-package :dbi-cp-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dbi-cp)' in your Lisp.

(defun connected (connection-pool)
  "sum of connected"
  (loop for pool across (slot-value connection-pool 'dbi-cp.connectionpool::pool)
        count (slot-value pool 'dbi-cp.connectionpool::connect-p)))

(defun available (connection-pool)
  "sum of available connection"
  (loop for pool across (slot-value connection-pool 'dbi-cp.connectionpool::pool)
        count (= (bt-sem:semaphore-count (slot-value pool 'dbi-cp.connectionpool::semaphore))
                 1)))

(defparameter *connection-pool-sqlite3* nil)
(defparameter *conn1* NIL)
(defparameter *conn2* NIL)
(defparameter *conn3* NIL)


(setup
  (setf *connection-pool-sqlite3*
    (make-dbi-connection-pool :sqlite3
                              :database-name ":memory:"
                              :initial-size 2
                              :max-size 3)))

(teardown
  (shutdown *connection-pool-sqlite3*))


(deftest check-connection
  (ok (eq (connected *connection-pool-sqlite3*) 2)
      "check initial connection size is 2")

  (ok (eq (available *connection-pool-sqlite3*) 3)
      "check initial avaiable connection size is 3"))

(deftest connection
  (setf *conn1* (get-connection *connection-pool-sqlite3*))

  (ok (eq (connected *connection-pool-sqlite3*) 2))
  (ok (eq (available *connection-pool-sqlite3*) 2))


  ;; get connection
  (setf *conn2* (get-connection *connection-pool-sqlite3*))

  (ok (eq (connected *connection-pool-sqlite3*) 2))
  (ok (eq (available *connection-pool-sqlite3*) 1))


  ;; new connection
  (setf *conn3* (get-connection *connection-pool-sqlite3*))

  (ok (eq (connected *connection-pool-sqlite3*) 3))
  (ok (eq (available *connection-pool-sqlite3*) 0))


  ;; no connection available
  (ok (signals (get-connection *connection-pool-sqlite3*) '<dbi-cp-no-connection>))


  ;; return connection
  ;; not disconnect database, connection continues
  (disconnect *conn1*)
  (ok (eq (connected *connection-pool-sqlite3*) 3))
  (ok (eq (available *connection-pool-sqlite3*) 1)))



