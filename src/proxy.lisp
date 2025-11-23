(in-package :cl-user)
(defpackage dbi-cp.proxy
  (:use :cl
        :cl-annot
        :annot.class)
  (:import-from #:cl-dbi
                #:disconnect
                #:prepare
                #:do-sql
                #:row-count
                #:begin-transaction
                #:commit
                #:rollback
                #:execute))

(in-package :dbi-cp.proxy)

(cl-syntax:use-syntax :annot)


(defannotation proxy (def-form)
    (:arity 1)
  "just a mark"
  def-form)

@export
@export-accessors
(defclass <dbi-connection-proxy> ()
  ((dbi-connection :type <dbi-connection>
                   :initarg :dbi-connection
                   :accessor dbi-connection)
   (begin-transaction-called :type boolean
                             :initform nil
                             :documentation "Flag indicating if begin-transaction was called")
   (initial-auto-commit :type boolean
                        :initarg :initial-auto-commit
                        :documentation "Initial value of auto-commit flag")
   (disconnect-fn :type function
                  :initarg :disconnect-fn
                  :accessor disconnect-fn)
   (connection-pool :type t
                    :initform nil
                    :initarg :connection-pool
                    :accessor connection-pool
                    :documentation "Reference to the parent connection pool")))

@proxy
(defmethod disconnect ((conn <dbi-connection-proxy>))
  (let ((begin-transaction-called (slot-value conn 'begin-transaction-called))
        (initial-auto-commit (slot-value conn 'initial-auto-commit))
        (dbi-connection (dbi-connection conn))
        (disconnect-fn (disconnect-fn conn)))

    ;; Rollback only if begin-transaction was called without commit/rollback
    (when begin-transaction-called
      (handler-case
          (do-sql dbi-connection "ROLLBACK")
        (error (e)
          ;; Ignore errors (already outside transaction, etc.)
          (declare (ignore e))))
      (setf (slot-value conn 'begin-transaction-called) nil))

    ;; Restore auto-commit flag to initial value
    (setf (slot-value dbi-connection 'dbi.driver::auto-commit)
          initial-auto-commit)

    ;; Return to pool
    (funcall disconnect-fn)))

@proxy
(defmethod prepare ((conn <dbi-connection-proxy>) (sql string) &rest rest &key &allow-other-keys)
  (let ((dbi-connection (dbi-connection conn)))
    (apply #'prepare dbi-connection sql rest)))

@proxy
(defmethod do-sql ((conn <dbi-connection-proxy>) (sql string) &optional params)
  (let ((dbi-connection (dbi-connection conn)))
    (do-sql dbi-connection sql params)))

@export
(defmacro with-transaction (conn &body body)
  `(unwind-protect
       (dbi:with-transaction (dbi-cp.proxy::dbi-connection ,conn)
         ,@body)
     ;; Clear begin-transaction-called flag on exit
     ;; because commit/rollback was executed by cl-dbi
     (setf (slot-value ,conn 'dbi-cp.proxy::begin-transaction-called) nil)))

@proxy
(defmethod row-count ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (row-count dbi-connection)))


@proxy
(defmethod begin-transaction ((conn <dbi-connection-proxy>))
  ;; Warn that begin-transaction is not recommended
  (warn "begin-transaction is not recommended with cl-dbi-connection-pool. ~
         Use with-transaction instead.")
  ;; Set flag to track begin-transaction was called
  (setf (slot-value conn 'begin-transaction-called) t)
  (let ((dbi-connection (dbi-connection conn)))
    ;; Call the actual begin-transaction
    (begin-transaction dbi-connection)))


@proxy
(defmethod commit ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (commit dbi-connection)))

@proxy
(defmethod rollback ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (rollback dbi-connection)))

@export
(defun get-max-allowed-packet (conn)
  "Get the MySQL max_allowed_packet value in bytes.
   Returns NIL for non-MySQL connections or if the value is not available."
  (when (typep conn '<dbi-connection-proxy>)
    (let ((pool (connection-pool conn)))
      (when pool
        (funcall (find-symbol "MAX-ALLOWED-PACKET" "DBI-CP.CONNECTIONPOOL") pool)))))

@export
(defun check-packet-size (conn sql &optional params)
  "Check if the query size is within max_allowed_packet limit.
   Returns T if within limit, NIL if exceeds limit or limit is not available."
  (let ((max-packet (get-max-allowed-packet conn)))
    (if max-packet
        (let ((estimated-size (estimate-query-size sql params)))
          (<= estimated-size max-packet))
        t)))

(defun estimate-query-size (sql params)
  "Estimate the size of a query in bytes.
   This is a conservative estimate."
  (let ((sql-size (length (babel:string-to-octets sql :encoding :utf-8)))
        (params-size 0))
    (when params
      (dolist (param params)
        (incf params-size
              (cond
                ((stringp param)
                 (length (babel:string-to-octets param :encoding :utf-8)))
                ((vectorp param)
                 (length param))
                ((numberp param)
                 20)
                (t
                 100)))))
    (+ sql-size params-size)))

@export
(defun packet-size-exceeded-p (conn sql &optional params)
  "Check if the query size exceeds max_allowed_packet limit.
   Returns T if exceeds, NIL otherwise."
  (let ((max-packet (get-max-allowed-packet conn)))
    (if max-packet
        (let ((estimated-size (estimate-query-size sql params)))
          (> estimated-size max-packet))
        nil)))

(defmethod execute :before ((query dbi.driver:<dbi-query>) &optional params)
  "Warn if query size is approaching max_allowed_packet limit"
  (let* ((connection (slot-value query 'dbi.driver::connection))
         (proxy (find-connection-proxy connection)))
    (when proxy
      (let ((max-packet (get-max-allowed-packet proxy)))
        (when max-packet
          (let* ((sql (slot-value query 'dbi.driver::sql))
                 (estimated-size (estimate-query-size sql params))
                 (threshold (* max-packet 0.9)))
            (when (> estimated-size threshold)
              (warn "Query size (~A bytes) is approaching max_allowed_packet (~A bytes)~%Query: ~A"
                    estimated-size max-packet sql))))))))

(defvar *connection-proxy-map* (make-hash-table :test 'eq :weakness :key)
  "Weak hash table mapping DBI connections to their proxies")

(defun register-connection-proxy (dbi-connection proxy)
  "Register a DBI connection to proxy mapping"
  (setf (gethash dbi-connection *connection-proxy-map*) proxy))

(defun unregister-connection-proxy (dbi-connection)
  "Unregister a DBI connection to proxy mapping"
  (remhash dbi-connection *connection-proxy-map*))

(defun find-connection-proxy (dbi-connection)
  "Find the connection proxy for a given DBI connection"
  (gethash dbi-connection *connection-proxy-map*))
