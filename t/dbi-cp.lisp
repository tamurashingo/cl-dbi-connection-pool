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


  ;; no connection available (will wait for timeout)
  (ok (signals (get-connection *connection-pool-sqlite3*) '<dbi-cp-no-connection>))


  ;; return connection
  ;; not disconnect database, connection continues
  (disconnect *conn1*)
  (ok (eq (connected *connection-pool-sqlite3*) 3))
  (ok (eq (available *connection-pool-sqlite3*) 1)))

(deftest checkout-timeout-immediate
  (testing "Connection acquired immediately when available"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 2
                                           :max-size 2
                                           :checkout-timeout 5))
           (start-time (get-internal-real-time))
           (conn (get-connection pool))
           (elapsed-time (/ (- (get-internal-real-time) start-time)
                           internal-time-units-per-second)))
      (ok conn "Connection should be acquired")
      (ok (< elapsed-time 0.5) "Connection should be acquired immediately (< 0.5s)")
      (disconnect conn)
      (shutdown pool))))

(deftest checkout-timeout-wait-and-acquire
  (testing "Connection acquired after waiting when returned by another thread"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 1
                                           :max-size 1
                                           :checkout-timeout 5))
           (conn1 (get-connection pool))
           (thread-result nil)
           (start-time nil))
      ;; Start a thread that will try to get a connection (will wait)
      (let ((thread (bt:make-thread
                     (lambda ()
                       (setf start-time (get-internal-real-time))
                       (handler-case
                           (let ((conn2 (get-connection pool)))
                             (setf thread-result :success)
                             (disconnect conn2))
                         (error (e)
                           (setf thread-result (format nil "Error: ~A" e))))))))
        ;; Wait a bit to ensure the thread is waiting
        (sleep 0.5)
        ;; Return the first connection after 1 second
        (sleep 0.5)
        (disconnect conn1)
        ;; Wait for the thread to complete
        (bt:join-thread thread)
        (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))
          (ok (eq thread-result :success) "Thread should acquire connection")
          (ok (< elapsed-time 5) "Connection should be acquired before timeout")))
      (shutdown pool))))

(deftest checkout-timeout-expires
  (testing "Timeout occurs when no connection becomes available"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 1
                                           :max-size 1
                                           :checkout-timeout 2))
           (conn1 (get-connection pool)))
      ;; All connections are in use, timeout should occur
      (let ((start-time (get-internal-real-time)))
        (ok (signals (get-connection pool) '<dbi-cp-no-connection>)
            "Should throw <dbi-cp-no-connection> error")
        (let ((elapsed-time (/ (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))
          (ok (>= elapsed-time 2) "Should wait at least 2 seconds")
          (ok (<= elapsed-time 3) "Should timeout around 2 seconds")))
      (disconnect conn1)
      (shutdown pool))))

(deftest checkout-timeout-custom-value
  (testing "Custom checkout-timeout value is respected"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 1
                                           :max-size 1
                                           :checkout-timeout 1)))
      (ok (= (checkout-timeout pool) 1)
          "Checkout timeout should be set to 1 second")
      (shutdown pool))))

(deftest checkout-timeout-concurrent-requests
  (testing "Multiple concurrent requests with timeout"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 2
                                           :max-size 2
                                           :checkout-timeout 10))
           (threads nil)
           (success-count 0)
           (lock (bt:make-lock)))
      ;; Create 5 threads competing for 2 connections
      (dotimes (i 5)
        (push (bt:make-thread
               (lambda ()
                 (handler-case
                     (let ((conn (get-connection pool)))
                       (sleep 0.5) ; Hold connection briefly
                       (disconnect conn)
                       (bt:with-lock-held (lock)
                         (incf success-count)))
                   (error (e)
                     (format t "Thread error: ~A~%" e)))))
              threads))
      ;; Wait for all threads to complete
      (dolist (thread threads)
        (bt:join-thread thread))
      (ok (= success-count 5) "All threads should eventually acquire connections")
      (shutdown pool))))

