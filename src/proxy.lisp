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
   (dirty :initform nil)
   (auto-commit :initform nil)
   (disconnect-fn :type function
                  :initarg :disconnect-fn
                  :accessor disconnect-fn)))

@proxy
(defmethod disconnect ((conn <dbi-connection-proxy>))
  (let ((dirty (slot-value conn 'dirty))
        (auto-commit (slot-value conn 'auto-commit))
        (disconnect-fn (disconnect-fn conn)))
    (when (and (not auto-commit)
               dirty)
      (rollback conn))
    (funcall disconnect-fn)))

@proxy
(defmethod prepare ((conn <dbi-connection-proxy>) (sql string) &rest rest &key &allow-other-keys)
  (let ((dbi-connection (dbi-connection conn)))
    (apply #'prepare dbi-connection sql rest)))

@proxy
(defmethod do-sql ((conn <dbi-connection-proxy>) (sql string) &optional params)
  (let ((dbi-connection (dbi-connection conn)))
    (do-sql dbi-connection sql params)))


(defmethod get-autocommit ((conn <dbi-connection-proxy>) &optional (not-supported-error-p t))
  (let ((dbi-conn (dbi-connection conn)))
    (get-autocommit-impl dbi-conn not-supported-error-p)))

(defmethod set-autocommit ((conn <dbi-connection-proxy>) mode &optional (not-supported-error-p t))
  (set-autocommit-impl (dbi-connection conn) mode not-supported-error-p))

(defgeneric set-autocommit-impl (dbi-conn mode not-supported-error-p)
  (:documentation ""))

@export
(defmacro with-transaction (conn &body body)
  (let ((conn-var (gensym "CONN-VAR")))
    `(let ((,conn-var (dbi-cp.proxy::dbi-connection ,conn)))
       (dbi:with-transaction ,conn-var
         ,@body))))

@proxy
(defmethod row-count ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (row-count dbi-connection)))


@proxy
(defmethod begin-transaction ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (begin-transaction dbi-connection)))


@proxy
(defmethod commit ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (commit dbi-connection)))

@proxy
(defmethod rollback ((conn <dbi-connection-proxy>))
  (let ((dbi-connection (dbi-connection conn)))
    (rollback dbi-connection)))
