(in-package :cl-user)
(defpackage dbi-cp-test.semaphore
  (:use :cl
        :prove
        :dbi-cp.semaphore))
(in-package :dbi-cp-test.semaphore)

(plan 7)

;; ----------------------------------------
;; dbi-semaphore
;; ----------------------------------------
(defparameter *SEM* (make-dbi-semaphore :connection 'CONNECTION))
(defparameter *CONN* NIL)

(ok (borrow-p *SEM*)
    "can borrow the connection")

(let ((conn (borrow *SEM*)))
  (is conn
      'CONNECTION
      "borrow the connection"))

(ok (not (borrow-p *SEM*))
    "can't borrow the connection")

(let ((conn (borrow *SEM*)))
  (isnt conn
        'CONNECTION
        "could not borrow the connection"))

(back *SEM*)

(ok (borrow-p *SEM*)
    "can borrow the connection")


;; ----------------------------------------
;; synchronized
;; ----------------------------------------
(defparameter *LOCK* (make-instance '<synchronized>))

;; without synchronized
(defparameter *val* 0)
(let ((threads (make-array 10)))
  (loop for i from 0 below 10
        do (setf (aref threads i)
                 (bt:make-thread
                  (lambda ()
                    (let ((v *val*))
                      (sleep 1)
                      (setf *val* (1+ v)))))))
  (loop for i from 0 below 10
        do (bt:join-thread (aref threads i))))


(isnt *val* 10
      "non synchronized")


;; with synchronized
(defparameter *val* 0)
(let ((threads (make-array 10)))
  (loop for i from 0 below 10
        do (setf (aref threads i)
                 (bt:make-thread
                  (lambda ()
                    (synchronized *LOCK*
                      (let ((v *val*))
                        (sleep 1)
                        (setf *val* (1+ v))))))))
  (loop for i from 0 below 10
        do (bt:join-thread (aref threads i))))

(is *val* 10
    "synchronized")


(finalize)
