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

(deftest idle-timeout-removes-idle-connections
  (testing "Idle connections are removed after idle-timeout"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 5
                                          :idle-timeout 3
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Use all 5 connections
             (let ((conns (loop for i from 0 below 5
                               collect (get-connection pool))))
               (ok (= (connected pool) 5) "Should have 5 active connections")
               ;; Return all connections
               (dolist (conn conns)
                 (disconnect conn)))

             ;; Wait for idle-timeout + reaper interval
             (ok (= (connected pool) 5) "Immediately after return, still 5 connections")
             (sleep 25) ; idle-timeout(3s) + reaper-interval(20s) + margin(2s)

             ;; Only initial-size connections should remain
             (ok (= (connected pool) 2) "After idle-timeout, only 2 connections remain"))
        (shutdown pool)))))

(deftest idle-timeout-maintains-initial-size
  (testing "Connection count never drops below initial-size"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 3
                                          :max-size 5
                                          :idle-timeout 3
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Initially should have initial-size connections
             (ok (= (connected pool) 3) "Should start with 3 connections")

             ;; Use and return one connection
             (let ((conn (get-connection pool)))
               (disconnect conn))

             ;; Wait for idle-timeout
             (sleep 25)

             ;; Should still have initial-size connections
             (ok (= (connected pool) 3) "Should maintain 3 connections (initial-size)"))
        (shutdown pool)))))

(deftest idle-timeout-preserves-recently-used-connections
  (testing "Recently used connections are not removed"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 5
                                          :idle-timeout 10
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Create 5 connections
             (let ((conns (loop for i from 0 below 5
                               collect (get-connection pool))))
               (dolist (conn conns)
                 (disconnect conn)))

             (ok (= (connected pool) 5) "Should have 5 connections")

             ;; Wait 5 seconds (less than idle-timeout)
             (sleep 5)

             ;; Use one connection to refresh its last-used-time
             (let ((conn (get-connection pool)))
               (disconnect conn))

             ;; Wait another 21 seconds (total 26s > idle-timeout(10s) + reaper-interval(20s) for old connections)
             (sleep 21)

             ;; At least one connection should remain (the recently used one + initial-size)
             (ok (>= (connected pool) 2) "Recently used connection should remain"))
        (shutdown pool)))))

(deftest idle-timeout-does-not-remove-in-use-connections
  (testing "Connections in use are not removed by reaper"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 5
                                          :idle-timeout 3
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Get all 5 connections
             (let ((conns (loop for i from 0 below 5
                               collect (get-connection pool))))
               (ok (= (connected pool) 5) "Should have 5 connections")
               (ok (= (available pool) 0) "All connections should be in use")

               ;; Wait for idle-timeout
               (sleep 25)

               ;; All connections should still exist (they're in use)
               (ok (= (connected pool) 5) "All 5 connections should remain (in use)")

               ;; Return all connections
               (dolist (conn conns)
                 (disconnect conn))))
        (shutdown pool)))))

(deftest idle-timeout-disabled-when-zero
  (testing "idle-timeout=0 disables the reaper feature"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 5
                                          :idle-timeout 0
                                          :max-lifetime nil
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Check that reaper thread was not started
             (ok (null (slot-value pool 'dbi-cp.connectionpool::reaper-thread))
                 "Reaper thread should not be started when idle-timeout=0 and max-lifetime=nil")

             ;; Create and return connections
             (let ((conns (loop for i from 0 below 5
                               collect (get-connection pool))))
               (dolist (conn conns)
                 (disconnect conn)))

             (ok (= (connected pool) 5) "Should have 5 connections")

             ;; Wait (would be enough for reaper to run if it existed)
             (sleep 25)

             ;; All connections should still exist
             (ok (= (connected pool) 5) "All 5 connections should remain (reaper disabled)"))
        (shutdown pool)))))

(deftest idle-timeout-reaper-thread-stops-on-shutdown
  (testing "Reaper thread is properly stopped on shutdown"
    (let* ((pool (make-dbi-connection-pool :sqlite3
                                           :database-name ":memory:"
                                           :initial-size 2
                                           :max-size 5
                                           :idle-timeout 60
                                           :reaper-interval 20))
           (reaper-thread (slot-value pool 'dbi-cp.connectionpool::reaper-thread)))
      ;; Reaper thread should be running
      (ok reaper-thread "Reaper thread should be started")
      (ok (bt:thread-alive-p reaper-thread) "Reaper thread should be alive")

      ;; Shutdown pool
      (shutdown pool)

      ;; Reaper thread should be stopped
      (sleep 0.5) ; Give it time to terminate
      (ok (not (bt:thread-alive-p reaper-thread)) "Reaper thread should be stopped after shutdown"))))

(deftest max-lifetime-recreates-old-connections
  (testing "Connections exceeding max-lifetime are recreated"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 3
                                          :max-lifetime 3
                                          :idle-timeout 0
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Get initial connections and their created times
             (let* ((conn1 (get-connection pool))
                    (conn2 (get-connection pool))
                    (pooled-conn1 (find-if (lambda (pc)
                                            (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                conn1))
                                          (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (pooled-conn2 (find-if (lambda (pc)
                                            (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                conn2))
                                          (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (created-time1 (slot-value pooled-conn1 'dbi-cp.connectionpool::created-time))
                    (created-time2 (slot-value pooled-conn2 'dbi-cp.connectionpool::created-time)))

               (ok created-time1 "Connection 1 should have created-time")
               (ok created-time2 "Connection 2 should have created-time")

               ;; Return connections
               (disconnect conn1)
               (disconnect conn2)

               ;; Wait for max-lifetime to expire
               (sleep 4)

               ;; Get connections again - they should be recreated
               (let ((new-conn1 (get-connection pool)))
                 (let* ((new-pooled-conn1 (find-if (lambda (pc)
                                                     (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                         new-conn1))
                                                   (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                        (new-created-time1 (slot-value new-pooled-conn1 'dbi-cp.connectionpool::created-time)))
                   (ok (> new-created-time1 created-time1)
                       "Connection should be recreated with newer created-time")
                   (disconnect new-conn1)))))
        (shutdown pool)))))

(deftest max-lifetime-preserves-young-connections
  (testing "Connections within max-lifetime are not recreated"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 3
                                          :max-lifetime 10
                                          :idle-timeout 0
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             ;; Get connection and check created time
             (let* ((conn (get-connection pool))
                    (pooled-conn (find-if (lambda (pc)
                                           (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                               conn))
                                         (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (created-time (slot-value pooled-conn 'dbi-cp.connectionpool::created-time)))

               (ok created-time "Connection should have created-time")

               ;; Return and immediately get again (within max-lifetime)
               (disconnect conn)
               (sleep 1)

               (let ((new-conn (get-connection pool)))
                 (let* ((new-pooled-conn (find-if (lambda (pc)
                                                   (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                       new-conn))
                                                 (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                        (new-created-time (slot-value new-pooled-conn 'dbi-cp.connectionpool::created-time)))
                   (ok (= new-created-time created-time)
                       "Connection should not be recreated (same created-time)")
                   (disconnect new-conn)))))
        (shutdown pool)))))

(deftest max-lifetime-reaper-recreates-connections
  (testing "Reaper thread recreates connections exceeding max-lifetime"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 3
                                          :max-lifetime 3
                                          :idle-timeout 0
                                          :reaper-interval 2)))
      (unwind-protect
           (progn
             ;; Get all connections and their created times
             (let* ((conns (loop for i from 0 below 2
                                collect (get-connection pool)))
                    (created-times (mapcar (lambda (conn)
                                            (let ((pc (find-if (lambda (p)
                                                               (eq (slot-value p 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                                   conn))
                                                             (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list))))
                                              (slot-value pc 'dbi-cp.connectionpool::created-time)))
                                          conns)))

               ;; Return all connections
               (dolist (conn conns)
                 (disconnect conn))

               ;; Wait for max-lifetime + reaper-interval
               (sleep 6)

               ;; Check that connections were recreated by reaper
               (let* ((new-conns (loop for i from 0 below 2
                                      collect (get-connection pool)))
                      (new-created-times (mapcar (lambda (conn)
                                                  (let ((pc (find-if (lambda (p)
                                                                    (eq (slot-value p 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                                        conn))
                                                                  (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list))))
                                                    (slot-value pc 'dbi-cp.connectionpool::created-time)))
                                                new-conns)))

                 (ok (every (lambda (pair)
                             (> (cadr pair) (car pair)))
                           (mapcar #'list created-times new-created-times))
                     "All connections should be recreated by reaper")

                 (dolist (conn new-conns)
                   (disconnect conn)))))
        (shutdown pool)))))

(deftest max-lifetime-disabled-when-nil
  (testing "max-lifetime=NIL disables the feature"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 3
                                          :max-lifetime nil
                                          :idle-timeout 0
                                          :reaper-interval 20)))
      (unwind-protect
           (progn
             (ok (null (slot-value pool 'dbi-cp.connectionpool::max-lifetime))
                 "max-lifetime should be NIL")

             ;; Get connection and check created time
             (let* ((conn (get-connection pool))
                    (pooled-conn (find-if (lambda (pc)
                                           (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                               conn))
                                         (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                    (created-time (slot-value pooled-conn 'dbi-cp.connectionpool::created-time)))

               ;; Return and wait
               (disconnect conn)
               (sleep 2)

               ;; Get again - should not be recreated
               (let ((new-conn (get-connection pool)))
                 (let* ((new-pooled-conn (find-if (lambda (pc)
                                                   (eq (slot-value pc 'dbi-cp.connectionpool::dbi-connection-proxy)
                                                       new-conn))
                                                 (coerce (slot-value pool 'dbi-cp.connectionpool::pool) 'list)))
                        (new-created-time (slot-value new-pooled-conn 'dbi-cp.connectionpool::created-time)))
                   (ok (= new-created-time created-time)
                       "Connection should not be recreated when max-lifetime is NIL")
                   (disconnect new-conn)))))
        (shutdown pool)))))

(deftest max-lifetime-default-value
  (testing "max-lifetime default value is 1800 seconds"
    (let ((pool (make-dbi-connection-pool :sqlite3
                                          :database-name ":memory:"
                                          :initial-size 2
                                          :max-size 3)))
      (unwind-protect
           (ok (= (slot-value pool 'dbi-cp.connectionpool::max-lifetime) 1800)
               "Default max-lifetime should be 1800 seconds")
        (shutdown pool)))))

