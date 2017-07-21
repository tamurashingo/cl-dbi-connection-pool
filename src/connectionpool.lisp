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
               :documentation "function what connecto to database ")))

(defclass <pooled-connection> ()
  ((connect-p :type boolean
              :accessor connect-p
              :initform NIL
              :documentation "T when already connected database")
   (semaphore :accessor semaphore
              :initform (bt-sem:make-semaphore :count 1))
   (dbi-connection-proxy :type <dbi-connection-proxy>
                         :initarg :dbi-connection-proxy
                         :accessor dbi-connection-proxy
                         :initform NIL)))

@export
(defun make-dbi-connection-pool (driver-name &rest params &key database-name username password (initial-size 10) (max-size 10) &allow-other-keys)
  "make connection pool

Example
  (make-dbi-connection-pool :mysql :database-name \"dbicp\" :username \"root\" :password \"password\")"
  ;; remove addtional parameter for original dbi:connect argument
  (remf params :initial-size)
  (remf params :max-size)

  (let* ((pool (make-array max-size :initial-element NIL))
         (dbi-connection-pool
          (make-instance '<dbi-connection-pool>
                         :pool pool
                         :connect-fn (lambda ()
                                       (apply #'dbi:connect driver-name params)))))
    ;; create <pooled-connection> instance
    (%make-pooledconnection-array! pool max-size)
    ;; connect initial-size connection
    (%make-connection-array! dbi-connection-pool initial-size)
    dbi-connection-pool))


(defun %make-pooledconnection-array! (cp-array array-size)
  "create <pooled-connection> instance"
  (loop for idx from 0 below array-size
        do (let* ((dbi-cp (make-instance '<dbi-connection-proxy>))
                  (pooled-connection (make-instance '<pooled-connection>
                                                    :dbi-connection-proxy dbi-cp)))
             (setf (aref cp-array idx) pooled-connection))))

(defun %make-connection-array! (dbi-connection-pool connection-count)
  "create connection array"
  (let ((connect-fn (slot-value dbi-connection-pool 'connect-fn))
        (pool (slot-value dbi-connection-pool 'pool)))
    (loop for idx from 0 below connection-count
          do (let ((pooled-connection (aref pool idx)))
               (%make-connection! pooled-connection connect-fn)))))

(defun %make-connection! (pooled-connection connect-fn)
  "connect database and set parameters"
  (let ((semaphore (semaphore pooled-connection))
        (dbi-proxy (dbi-connection-proxy pooled-connection)))
    ;; connected
    (setf (connect-p pooled-connection) T)
    ;; make connection
    (setf (dbi-connection dbi-proxy) (funcall connect-fn))
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
  (loop for pool across (slot-value conn 'pool)
        if (connect-p pool)
          do (let ((semaphore (semaphore pool)))
               (when (bt-sem:try-semaphore semaphore)
                 (return (dbi-connection-proxy pool))))
        else
          do (let ((semaphore (semaphore pool)))
               (when (bt-sem:try-semaphore semaphore)
                 (%make-connection! pool (slot-value conn 'connect-fn))
                 (return (dbi-connection-proxy pool))))
        end
        finally (return (error '<dbi-cp-no-connection>))))
