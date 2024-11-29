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
   (in-transaction :initform 0)
   (disconnect-fn :type function
                  :initarg :disconnect-fn
                  :accessor disconnect-fn)))

@proxy
(defmethod disconnect ((conn <dbi-connection-proxy>))
  (let ((in-transaction (slot-value conn 'in-transaction))
        (disconnect-fn (disconnect-fn conn)))
    (when (not (= in-transaction 0))
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
  (incf (slot-value conn 'in-transaction))
  (let ((dbi-connection (dbi-connection conn)))
    (begin-transaction dbi-connection)))


@proxy
(defmethod commit ((conn <dbi-connection-proxy>))
  (decf (slot-value conn 'in-transaction))
  (let ((dbi-connection (dbi-connection conn)))
    (commit dbi-connection)))

@proxy
(defmethod rollback ((conn <dbi-connection-proxy>))
  (decf (slot-value conn 'in-transaction))
  (let ((dbi-connection (dbi-connection conn)))
    (rollback dbi-connection)))
