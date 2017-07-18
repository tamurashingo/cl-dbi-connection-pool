(in-package :cl-user)
(defpackage dbi-cp.connectionpool
  (:use :cl)
  (:import-from :dbi-cp.proxy
                :<dbi-connection-proxy>
                :dbi-connection
                :disconnect-fn
                :disconnect
                :prepare
                :do-sql))
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
              :initform NIL)
   (semaphore :accessor semaphore
              :initform (bt-sem:make-semaphore :count 1))
   (dbi-connection-proxy :type <dbi-connection-proxy>
                         :initarg :dbi-connection-proxy
                         :accessor dbi-connection-proxy
                         :initform NIL)))

@export
(defun make-dbi-connection-pool (driver-name &rest params &key database-name username password (initial-size 10) (max-size 10))
  "make connection pool

Example
  (make-connection-pool :mysql :database-name \"cldbi\" :username \"root\" :password \"password\")"
  (let* ((pool (make-array max-size :initial-element NIL))
         (dbi-connection-pool
          (make-instance '<dbi-connection-pool>
                         :pool pool
                         :connect-fn (lambda ()
                                       (apply #'dbi:connect driver-name params)))))
    ;; create <pooled-connection> instance
    (loop for idx from 0 below max-size
          do (setf (aref pool idx)
                   (make-instance '<pooled-connection>
                                  :dbi-connection-proxy (make-instance '<dbi-connection-proxy>))))
    ;; connect initial-size connection
    (let ((connect-fn (slot-value dbi-connection-pool 'connect-fn)))
      (loop for idx from 0 below initial-size
            do (let* ((pooled-conn (aref pool idx))
                      (semaphore (semaphore pooled-conn))
                      (dbi-connection-proxy (dbi-connection-proxy pooled-conn)))
                 (setf (connect-p pooled-conn) T)
                 (setf (dbi-connection dbi-connection-proxy) (funcall connect-fn))
                 (setf (disconnect-fn dbi-connection-proxy)
                       (lambda ()
                         (bt-sem:signal-semaphore semaphore))))))
    dbi-connection-pool))


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
                    (let ((dbi-connection-proxy (dbi-connection-proxy pool)))
                      (setf (disconnect-fn dbi-connection-proxy)
                            (lambda ()
                              (bt-sem:signal-semaphore semaphore)))
                      (return dbi-connection-proxy))))
        else
          do (let ((semaphore (semaphore pool)))
               (when (bt-sem:try-semaphore semaphore)
                 (let ((connect-fn (slot-value conn 'connect-fn))
                       (dbi-connection-proxy (dbi-connection-proxy conn)))
                   (setf (dbi-connection dbi-connection-proxy) (funcall connect-fn))
                   (setf (disconnect-fn dbi-connection-proxy)
                         (lambda ()
                           (bt-sem:signal-semaphore semaphore)))
                   (return dbi-connection-proxy))))
        end
        finally (return nil)))

