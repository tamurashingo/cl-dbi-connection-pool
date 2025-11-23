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
                #:rollback))

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
                  :accessor disconnect-fn)))

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
