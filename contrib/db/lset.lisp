;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; Wait-Free Set Implementation
;;;;
;;;; local customized divergent branch of sb-lset http://github.com/nikodemus/sb-lset/ 
;;;;
;;;; Based on "A Lazy Concurrent List-Based Set Algorithm" (2005) by Steve
;;;; Heller, Maurice Herlihy, Victor Luchangco, Mark Moir, William N. Scherer
;;;; III, and Nir Shavit.
;;;;

(defpackage :templeton.lset
  (:use :cl :sb-ext)
  (:export
   "MAKE-LSET"
   "ADD-ENTRY"
   "REMOVE-ENTRY"
   "CONTAINS-ENTRY-P"
   "MAP-ENTRIES"
   "LIST-ENTRIES"
   "COUNT-ENTRIES"))

(in-package :templeton.lset)

(defstruct entry
  key
  next
  marked
  lock)

(defstruct (lset (:constructor make-lset (&key lessp (test #'eql)
                                               &aux (tail (make-entry))
                                               (head (make-entry :next tail)))))
  head
  tail
  (lessp (sb-int:missing-arg) :type function)
  (test (sb-int:missing-arg) :type function))

(setf (documentation 'make-lset 'function)
      "Creates an empty LSET object, representing a set. LESSP function must provide total
ordering over the set, whereas TEST function is used to test for equivalance.

Example:

  ;; Set of EVENT objects with timestamps. Since two timestamps might
  ;; be equal we us an additional unique ID to break ties.
  (make-lset :test #'eq :lessp (lambda (a b)
                                 (let ((ta (event-timestamp a))
                                       (tb (event-timestamp b)))
                                   (or (timestamp< ta tb)
                                       (and (timestamp= ta tb)
                                            (< (event-id a) (event-id b)))))))
")

(defun validate (pred curr)
  (and (not (entry-marked pred))
       (not (entry-marked curr))
       (eq curr (entry-next pred))))

(defun call-with-entry-lock (function entry)
  (declare (function function))
  (let ((me sb-thread:*current-thread*))
    (if (eq me (entry-lock entry))
        (funcall function)
        (sb-sys:without-interrupts
          (unwind-protect
               (sb-sys:with-local-interrupts
                 ;; LOCK -- volatile reads followed by CAS/yield.
                 (loop
                   (loop repeat 1024
                         while (entry-lock entry))
                   (unless (entry-lock entry)
                     (if (sb-ext:compare-and-swap (entry-lock entry) nil me)
                         (sb-thread:thread-yield)
                         (return t))))
                 ;; ACTION.
                 (funcall function))
            ;; UNLOCK.
            (sb-ext:compare-and-swap (entry-lock entry) me nil))))))

(defmacro with-entry-lock ((entry) &body body)
  `(flet ((exec () ,@body))
     (declare (dynamic-extent #'exec))
     (call-with-entry-lock #'exec ,entry)))

(defmacro with-optimistic-walk ((pred curr key lset)
                                &body cases)
  (sb-int:with-unique-names (lessp test head tail list value)
    `(let* ((,list ,lset)
            (,value ,key)
            (,lessp (lset-lessp ,list))
            (,test (lset-test ,list))
            (,head (lset-head ,list))
            (,tail (lset-tail ,list)))
       (loop
         (let* ((,pred ,head)
                (,curr (entry-next ,pred)))
           (loop while (and (not (eq ,tail ,curr))
                            (funcall ,lessp (entry-key ,curr) ,value))
                 do (setf ,pred ,curr
                          ,curr (entry-next ,curr)))
           (with-entry-lock (,pred)
             (with-entry-lock (,curr)
               (when (validate ,pred ,curr)
                 (cond ((or (eq ,tail ,curr)
                            (not (funcall ,test ,key (entry-key ,curr))))
                        ,@(or (cdr (assoc :missing cases))
                              (error "Missing :MISSING case.")))
                       (t
                        ,@(or (cdr (assoc :found cases))
                              (error "Missing :FOUND case."))))))))))))

(defun remove-entry (key lset)
  (with-optimistic-walk (pred curr key lset)
    (:missing
     ;; Not in the list.
     (return-from remove-entry nil))
    (:found
     ;; Removed first logically, then physically.
     (setf (entry-marked curr) t)
     (setf (entry-next pred) (entry-next curr))
     (return-from remove-entry t))))

(defun add-entry (key lset)
  (with-optimistic-walk (pred curr key lset)
    (:missing
     ;; Not in list, new entry.
     (let ((new (make-entry :key key :next curr)))
       (setf (entry-next pred) new)
       (return-from add-entry t)))
    (:found
     ;; Already in list.
     (return-from add-entry nil))))

(defun contains-entry-p (key lset)
  (let* ((curr (entry-next (lset-head lset)))
         (tail (lset-tail lset))
         (lessp (lset-lessp lset)))
    (loop while (and (not (eq tail curr))
                     (funcall lessp (entry-key curr) key))
          do (setf curr (entry-next curr)))
    (when (and (not (eq tail curr))
               (not (entry-marked curr))
               (funcall (lset-test lset) (entry-key curr) key))
      t)))

(defun map-entries (function lset)
  (let* ((curr (entry-next (lset-head lset)))
         (tail (lset-tail lset)))
    (loop while (not (eq tail curr))
          do (unless (entry-marked curr)
               (funcall function (entry-key curr)))
             (setf curr (entry-next curr)))))

(defun list-entries (lset)
  (let (all)
    (map-entries (lambda (x) (push x all)) lset)
    (nreverse all)))

(defun count-entries (lset)
  (let ((n 0))
    (map-entries (lambda (x) (declare (ignore x)) (incf n)) lset)
    n))
