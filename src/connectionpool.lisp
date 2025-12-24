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
                     :documentation "Maximum wait time (in seconds) when acquiring a connection from the pool")
   (initial-size :type integer
                 :initarg :initial-size
                 :accessor initial-size
                 :documentation "Minimum number of connections to maintain in the pool")
   (idle-timeout :type integer
                 :initarg :idle-timeout
                 :initform 600
                 :accessor idle-timeout
                 :documentation "Time in seconds after which idle connections are removed from the pool")
   (max-lifetime :type (or null integer)
                 :initarg :max-lifetime
                 :initform 1800
                 :accessor max-lifetime
                 :documentation "Maximum lifetime in seconds for a connection since creation. NIL disables this feature")
   (keepalive-interval :type integer
                       :initarg :keepalive-interval
                       :initform 0
                       :accessor keepalive-interval
                       :documentation "Interval in seconds for checking connection validity. 0 disables keepalive checks")
   (validation-query :type (or null string)
                     :initarg :validation-query
                     :initform nil
                     :accessor validation-query
                     :documentation "Query used to validate connection (e.g., 'SELECT 1'). Required for keepalive to work")
   (reaper-interval :type integer
                    :initarg :reaper-interval
                    :initform 60
                    :accessor reaper-interval
                    :documentation "Interval in seconds between reaper thread executions")
   (reaper-thread :type (or null bt:thread)
                  :initform nil
                  :accessor reaper-thread
                  :documentation "Background thread that removes idle connections")))

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
                         :initform NIL)
   (last-used-time :type (or null integer)
                   :initform nil
                   :accessor last-used-time
                   :documentation "Last time this connection was used (universal-time)")
   (created-time :type (or null integer)
                 :initform nil
                 :accessor created-time
                 :documentation "Time when this connection was created (universal-time)")
   (last-keepalive-time :type (or null integer)
                        :initform nil
                        :accessor last-keepalive-time
                        :documentation "Last time keepalive check was performed (universal-time)")))

@export
(defun make-dbi-connection-pool (driver-name &rest params &key database-name username password (initial-size 10) (max-size 10) (checkout-timeout 30) (idle-timeout 600) (max-lifetime 1800) (keepalive-interval 0) validation-query (reaper-interval 60) &allow-other-keys)
  "make connection pool

Example
  (make-dbi-connection-pool :mysql :database-name \"dbicp\" :username \"root\" :password \"password\")"
  ;; remove addtional parameter for original dbi:connect argument
  (remf params :initial-size)
  (remf params :max-size)
  (remf params :checkout-timeout)
  (remf params :idle-timeout)
  (remf params :max-lifetime)
  (remf params :keepalive-interval)
  (remf params :validation-query)
  (remf params :reaper-interval)

  (let* ((pool (make-array max-size :initial-element NIL))
         (dbi-connection-pool
          (make-instance '<dbi-connection-pool>
                         :pool pool
                         :driver-name driver-name
                         :checkout-timeout checkout-timeout
                         :initial-size initial-size
                         :idle-timeout idle-timeout
                         :max-lifetime max-lifetime
                         :keepalive-interval keepalive-interval
                         :validation-query validation-query
                         :reaper-interval reaper-interval
                         :connect-fn (lambda ()
                                       (apply #'dbi:connect driver-name params)))))
    ;; create <pooled-connection> instance
    (%make-pooledconnection-array! pool max-size dbi-connection-pool)
    ;; connect initial-size connection
    (%make-connection-array! dbi-connection-pool initial-size)

    ;; Retrieve max_allowed_packet for MySQL
    (when (eq driver-name :mysql)
      (%retrieve-max-allowed-packet! dbi-connection-pool))

    ;; Retrieve wait_timeout for MySQL and adjust max-lifetime if needed
    (when (and (eq driver-name :mysql) max-lifetime)
      (%retrieve-and-adjust-max-lifetime! dbi-connection-pool max-lifetime))

    ;; Warn if keepalive-interval is set but validation-query is not provided
    (when (and (> keepalive-interval 0) (not validation-query))
      (warn "keepalive-interval (~A seconds) is set but validation-query is not provided. Keepalive checks will not be performed. Please set :validation-query to enable keepalive."
            keepalive-interval))

    ;; Start reaper thread if idle-timeout, max-lifetime, or keepalive-interval is enabled
    (when (or (> idle-timeout 0) max-lifetime (and (> keepalive-interval 0) validation-query))
      (%start-reaper-thread dbi-connection-pool))

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
    ;; record creation time
    (setf (created-time pooled-connection) (get-universal-time))
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
            (setf (last-used-time pooled-connection) (get-universal-time))
            (bt-sem:signal-semaphore semaphore)))))

@export
(defmethod shutdown ((conn <dbi-connection-pool>))
  "disconnect all connections"
  ;; Stop reaper thread first
  (when (reaper-thread conn)
    (bt:destroy-thread (reaper-thread conn))
    (setf (reaper-thread conn) nil))
  ;; Disconnect all connections
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
        (sleep-interval 0.1)
        (max-lifetime (max-lifetime conn))
        (current-time (get-universal-time)))
    (loop
      ;; Attempt to acquire connection
      (loop for pool across (slot-value conn 'pool)
            if (connect-p pool)
              do (let ((semaphore (semaphore pool)))
                   (when (bt-sem:try-semaphore semaphore)
                     ;; Check max-lifetime
                     (if (and max-lifetime
                              (created-time pool)
                              (> (- current-time (created-time pool)) max-lifetime))
                         ;; Connection exceeded max-lifetime, recreate it
                         (progn
                           (%recreate-connection! pool conn)
                           (return-from get-connection (dbi-connection-proxy pool)))
                         ;; Connection is valid
                         (return-from get-connection (dbi-connection-proxy pool)))))
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

(defun %retrieve-and-adjust-max-lifetime! (dbi-connection-pool specified-max-lifetime)
  "Retrieve wait_timeout from MySQL server and adjust max-lifetime if needed"
  (handler-case
      (let ((proxy (get-connection dbi-connection-pool)))
        (unwind-protect
             (let* ((dbi-connection (dbi-connection proxy))
                    (query (dbi:prepare dbi-connection
                                        "SHOW VARIABLES LIKE 'wait_timeout'"))
                    (result (dbi:execute query))
                    (row (dbi:fetch result)))
               (when row
                 (let* ((wait-timeout (parse-integer (getf row :|Value|)))
                        ;; Recommended: wait_timeout - 60 seconds
                        (recommended-max-lifetime (max 60 (- wait-timeout 60))))
                   (when (> specified-max-lifetime wait-timeout)
                     (warn "max-lifetime (~A seconds) is greater than MySQL wait_timeout (~A seconds). Using ~A seconds (wait_timeout - 60) instead."
                           specified-max-lifetime wait-timeout recommended-max-lifetime)
                     (setf (max-lifetime dbi-connection-pool) recommended-max-lifetime))
                   (when (and (> specified-max-lifetime recommended-max-lifetime)
                              (<= specified-max-lifetime wait-timeout))
                     (warn "max-lifetime (~A seconds) is close to MySQL wait_timeout (~A seconds). Consider using ~A seconds (wait_timeout - 60) or less."
                           specified-max-lifetime wait-timeout recommended-max-lifetime)))))
          (disconnect proxy)))
    (error (e)
      (warn "Failed to retrieve wait_timeout: ~A" e))))

(defun %start-reaper-thread (dbi-connection-pool)
  "Start background thread to reap idle connections"
  (setf (reaper-thread dbi-connection-pool)
        (bt:make-thread
         (lambda ()
           (%reaper-loop dbi-connection-pool))
         :name "dbi-cp-reaper")))

(defun %reaper-loop (dbi-connection-pool)
  "Reaper thread main loop - checks for idle connections periodically"
  (let ((interval (reaper-interval dbi-connection-pool)))
    (loop
      (sleep interval)
      (handler-case
          (%reap-idle-connections dbi-connection-pool)
        (error (e)
          (warn "Reaper thread error: ~A" e))))))

(defun %reap-idle-connections (dbi-connection-pool)
  "Remove idle connections exceeding idle-timeout and perform keepalive checks"
  (let* ((pool (slot-value dbi-connection-pool 'pool))
         (idle-timeout (idle-timeout dbi-connection-pool))
         (max-lifetime (max-lifetime dbi-connection-pool))
         (keepalive-interval (keepalive-interval dbi-connection-pool))
         (validation-query (validation-query dbi-connection-pool))
         (initial-size (initial-size dbi-connection-pool))
         (current-time (get-universal-time))
         (active-count 0))

    ;; Count active connections
    (loop for pc across pool
          when (connect-p pc)
            do (incf active-count))

    ;; Perform keepalive checks on idle connections
    (when (and (> keepalive-interval 0) validation-query)
      (loop for pc across pool
            when (and (connect-p pc)
                      (or (null (last-keepalive-time pc))
                          (> (- current-time (last-keepalive-time pc)) keepalive-interval)))
              do (when (bt-sem:try-semaphore (semaphore pc))
                   (unwind-protect
                        (progn
                          (if (%validate-connection pc dbi-connection-pool)
                              ;; Connection is valid, update keepalive time
                              (setf (last-keepalive-time pc) current-time)
                              ;; Connection is invalid, recreate it
                              (progn
                                (warn "Connection validation failed, recreating connection")
                                (%recreate-connection! pc dbi-connection-pool)
                                (setf (last-keepalive-time pc) current-time))))
                     (bt-sem:signal-semaphore (semaphore pc))))))

    ;; Check max-lifetime and recreate connections
    (when max-lifetime
      (loop for pc across pool
            when (and (connect-p pc)
                      (created-time pc)
                      (> (- current-time (created-time pc)) max-lifetime))
              do (when (bt-sem:try-semaphore (semaphore pc))
                   (%recreate-connection! pc dbi-connection-pool))))

    ;; Remove idle connections (but keep initial-size)
    (loop for pc across pool
          when (and (connect-p pc)
                    (> active-count initial-size)
                    (last-used-time pc)
                    (> (- current-time (last-used-time pc)) idle-timeout))
            do (when (bt-sem:try-semaphore (semaphore pc))
                 (%disconnect-pooled-connection pc)
                 (decf active-count)))))

(defun %disconnect-pooled-connection (pooled-connection)
  "Disconnect a pooled connection"
  (let* ((dbi-proxy (dbi-connection-proxy pooled-connection))
         (dbi-connection (dbi-connection dbi-proxy)))
    (when dbi-connection
      (handler-case
          (dbi:disconnect dbi-connection)
        (error (e)
          (warn "Error disconnecting pooled connection: ~A" e))))
    (setf (connect-p pooled-connection) nil)
    (setf (last-used-time pooled-connection) nil)
    (setf (created-time pooled-connection) nil)
    (setf (last-keepalive-time pooled-connection) nil)
    (bt-sem:signal-semaphore (semaphore pooled-connection))))

(defun %validate-connection (pooled-connection dbi-connection-pool)
  "Validate connection using validation-query"
  (handler-case
      (let* ((dbi-proxy (dbi-connection-proxy pooled-connection))
             (dbi-conn (dbi-connection dbi-proxy))
             (query-str (validation-query dbi-connection-pool)))
        (when (and dbi-conn query-str)
          (let ((query (dbi:prepare dbi-conn query-str)))
            (dbi:execute query)
            (dbi:fetch query))
          t))
    (error (e)
      (warn "Connection validation failed: ~A" e)
      nil)))

(defun %recreate-connection! (pooled-connection dbi-connection-pool)
  "Recreate a connection that exceeded max-lifetime or failed validation"
  (let* ((dbi-proxy (dbi-connection-proxy pooled-connection))
         (dbi-connection (dbi-connection dbi-proxy)))
    ;; Disconnect old connection
    (when dbi-connection
      (handler-case
          (dbi:disconnect dbi-connection)
        (error (e)
          (warn "Error disconnecting old connection: ~A" e))))
    ;; Create new connection
    (let ((connect-fn (slot-value dbi-connection-pool 'connect-fn)))
      (setf (created-time pooled-connection) (get-universal-time))
      (setf (last-used-time pooled-connection) nil)
      (setf (last-keepalive-time pooled-connection) nil)
      (let ((new-dbi-connection (funcall connect-fn)))
        (setf (dbi-connection dbi-proxy) new-dbi-connection)
        ;; Save initial auto-commit value
        (setf (slot-value dbi-proxy 'dbi-cp.proxy::initial-auto-commit)
              (slot-value new-dbi-connection 'dbi.driver::auto-commit))
        ;; Register proxy for connection
        (dbi-cp.proxy::register-connection-proxy new-dbi-connection dbi-proxy)))
    (bt-sem:signal-semaphore (semaphore pooled-connection))))
