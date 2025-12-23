(in-package :cl-user)
(defpackage dbi-cp.connectionpool
  (:use :cl)
  (:import-from :dbi-cp.proxy
                :<dbi-connection-proxy>
                :dbi-connection
                :disconnect-fn
                :disconnect
                :prepare
                :do-sql)
  (:import-from :dbi-cp.error
                :<dbi-cp-no-connection>))
(in-package :dbi-cp.connectionpool)

(cl-syntax:use-syntax :annot)


@export
(defclass <dbi-connection-pool> ()
  ((pool :type array
         :initarg :pool
         :documentation "array of <pooled-connection>")
   (connect-fn :type function
               :initarg :connect-fn
               :documentation "function what connecto to database ")
   (driver-name :type keyword
                :initarg :driver-name
                :accessor driver-name
                :documentation "Database driver name (:mysql, :postgres, :sqlite3, etc.)")
   (max-allowed-packet :type (or null integer)
                       :initform nil
                       :accessor max-allowed-packet
                       :documentation "MySQL max_allowed_packet value in bytes. NIL for non-MySQL connections")
   (checkout-timeout :type integer
                     :initarg :checkout-timeout
                     :initform 30
                     :accessor checkout-timeout
                     :documentation "Maximum wait time (in seconds) when acquiring a connection from the pool")))

(defclass <pooled-connection> ()
  ((connect-p :type boolean
              :accessor connect-p
              :initform NIL
              :documentation "T when already connected database")
   (semaphore :accessor semaphore
              :initform (bt-sem:make-semaphore :count 1))
   (dbi-connection-proxy :type (or null <dbi-connection-proxy>)
                         :initarg :dbi-connection-proxy
                         :accessor dbi-connection-proxy
                         :initform NIL)))

@export
(defun make-dbi-connection-pool (driver-name &rest params &key database-name username password (initial-size 10) (max-size 10) (checkout-timeout 30) &allow-other-keys)
  "make connection pool

Example
  (make-dbi-connection-pool :mysql :database-name \"dbicp\" :username \"root\" :password \"password\")"
  ;; remove addtional parameter for original dbi:connect argument
  (remf params :initial-size)
  (remf params :max-size)
  (remf params :checkout-timeout)

  (let* ((pool (make-array max-size :initial-element NIL))
         (dbi-connection-pool
          (make-instance '<dbi-connection-pool>
                         :pool pool
                         :driver-name driver-name
                         :checkout-timeout checkout-timeout
                         :connect-fn (lambda ()
                                       (apply #'dbi:connect driver-name params)))))
    ;; create <pooled-connection> instance
    (%make-pooledconnection-array! pool max-size dbi-connection-pool)
    ;; connect initial-size connection
    (%make-connection-array! dbi-connection-pool initial-size)

    ;; Retrieve max_allowed_packet for MySQL
    (when (eq driver-name :mysql)
      (%retrieve-max-allowed-packet! dbi-connection-pool))

    dbi-connection-pool))


(defun %make-pooledconnection-array! (cp-array array-size dbi-connection-pool)
  "create <pooled-connection> instance"
  (loop for idx from 0 below array-size
        do (let* ((dbi-cp (make-instance '<dbi-connection-proxy>
                                         :connection-pool dbi-connection-pool))
                  (pooled-connection (make-instance '<pooled-connection>
                                                    :dbi-connection-proxy dbi-cp)))
             (setf (aref cp-array idx) pooled-connection))))

(defun %make-connection-array! (dbi-connection-pool connection-count)
  "create connection array"
  (let ((pool (slot-value dbi-connection-pool 'pool)))
    (loop for idx from 0 below connection-count
          do (let ((pooled-connection (aref pool idx)))
               (%make-connection! pooled-connection dbi-connection-pool)))))

(defun %make-connection! (pooled-connection dbi-connection-pool)
  "connect database and set parameters"
  (let ((semaphore (semaphore pooled-connection))
        (dbi-proxy (dbi-connection-proxy pooled-connection))
        (connect-fn (slot-value dbi-connection-pool 'connect-fn)))
    ;; connected
    (setf (connect-p pooled-connection) T)
    ;; make connection
    (let ((dbi-connection (funcall connect-fn)))
      (setf (dbi-connection dbi-proxy) dbi-connection)
      ;; Save initial auto-commit value
      (setf (slot-value dbi-proxy 'dbi-cp.proxy::initial-auto-commit)
            (slot-value dbi-connection 'dbi.driver::auto-commit))
      ;; Register proxy for connection
      (dbi-cp.proxy::register-connection-proxy dbi-connection dbi-proxy))
    ;; make disconnect callback
    (setf (disconnect-fn dbi-proxy)
          (lambda ()
            (bt-sem:signal-semaphore semaphore)))))

@export
(defmethod shutdown ((conn <dbi-connection-pool>))
  "disconnect all connections"
  (loop for pool across (slot-value conn 'pool)
        when (connect-p pool)
             do (let* ((dbi-connection-proxy (dbi-connection-proxy pool))
                       (dbi-connection (dbi-connection dbi-connection-proxy)))
                  (disconnect dbi-connection))))

@export
(defmethod get-connection ((conn <dbi-connection-pool>))
  "get <dbi-connection-proxy> from connection pool"
  (let ((timeout-time (+ (get-internal-real-time)
                         (* (checkout-timeout conn) internal-time-units-per-second)))
        (sleep-interval 0.1))
    (loop
      ;; Attempt to acquire connection
      (loop for pool across (slot-value conn 'pool)
            if (connect-p pool)
              do (let ((semaphore (semaphore pool)))
                   (when (bt-sem:try-semaphore semaphore)
                     (return-from get-connection (dbi-connection-proxy pool))))
            else
              do (let ((semaphore (semaphore pool)))
                   (when (bt-sem:try-semaphore semaphore)
                     (%make-connection! pool conn)
                     (return-from get-connection (dbi-connection-proxy pool))))
            end)

      ;; Check timeout
      (when (>= (get-internal-real-time) timeout-time)
        (error '<dbi-cp-no-connection>))

      ;; Sleep briefly and retry
      (sleep sleep-interval))))

(defun %retrieve-max-allowed-packet! (dbi-connection-pool)
  "Retrieve max_allowed_packet from MySQL server"
  (handler-case
      (let ((proxy (get-connection dbi-connection-pool)))
        (unwind-protect
             (let* ((dbi-connection (dbi-connection proxy))
                    (query (dbi:prepare dbi-connection
                                        "SHOW VARIABLES LIKE 'max_allowed_packet'"))
                    (result (dbi:execute query))
                    (row (dbi:fetch result)))
               (when row
                 (setf (max-allowed-packet dbi-connection-pool)
                       (parse-integer (getf row :|Value|)))))
          (disconnect proxy)))
    (error (e)
      (warn "Failed to retrieve max_allowed_packet: ~A" e))))
