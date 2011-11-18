;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :templeton)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHASH / PINDEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(def (class* eas) phash ()
  ((btree :initarg :btree :accessor phash-btree))
  (:default-initargs :btree (ele:make-btree)))

(def (function e) make-phash (&optional (name (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root name)))
    (if btree
      (make-instance 'phash :map btree)
      (let ((phash (make-instance 'phash)))
        (ele:add-to-root name (phash-btree phash))
        phash))))

(def (class* eas) pindex (phash)
  ((btree :accessor pindex-btree))
  (:default-initargs :map (ele:make-indexed-btree)))

(def (function e) make-pindex (&optional (name (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root name)))
    (if btree
      (make-instance 'pindex :map btree)
      (let ((pindex (make-instance 'pindex)))
        (ele:add-to-root name (pindex-btree pindex))
        pindex))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexer Protocol: PHASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def method map-get ((phash phash) key)
  (ele:ensure-transaction ()
    (ele:get-value key (phash-btree phash))))

(def (function ed) phash-get (key phash)
  (map-get phash key))

(def method map-put ((phash phash) key value)
  (ele:ensure-transaction ()
    (setf (ele:get-value key (phash-btree phash)) value)))

(def (function ed) phash-put (key value phash)
  (map-put phash key value))

(def method map-ensure-get ((phash phash) key otherwise-put)
  (ele:ensure-transaction ()
    (or
      (values (map-get phash key) t)
      (values (map-put phash key otherwise-put) nil))))

(def (function ed) phash-get* (key phash otherwise-put)
  (map-ensure-get phash key otherwise-put))

(defsetf map-get map-put)
(defsetf phash-get phash-put)

(def method map-drop ((phash phash) key)
  (ele:ensure-transaction ()
    (remove-kv key (phash-btree phash))))

(def (function ed) phash-drop (key phash)
  (map-drop phash key))

(def method map-size ((phash phash))
  (let (count)
    (ele:ensure-transaction ()
      (map-btree (lambda (k v)
                   (declare (ignore k v))
                   (incf count))
        (phash-btree phash))
      count)))

(def (function ed) phash-size (phash)
  (map-size phash))
  
(def method map-p ((phash phash))
  (map-p (phash-btree phash)))

(def method map-p ((btree ele:btree))
  t)

(def method map-map  ((phash phash) fn/2)
  (ele:ensure-transaction ()
    (ele:map-btree fn/2 (phash-btree phash))))

(def (function ed) phash-map (fn phash)
  (map-map phash fn))

(def (function ed) phash-clear (phash)
  (setf (slot-value phash 'btree) (ele:make-btree)))

(def (function ed) pindex-clear (pindex)
  (setf (slot-value pindex 'btree) (ele:make-indexed-btree)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def suite* (test :in root-suite))

(def special-variable *phash*)

(def fixture |phash|
  (unwind-protect
    (let ((*phash*  (make-phash 'test-phash)))
      (-body-))
    (ele:drop-btree (phash-btree (make-phash 'test-phash)))))

(def test object-identity ()
  (with-fixture |phash|
    (is (not (eq (make-phash 'test-phash) (make-phash 'test-phash))))
    (is (eq (phash-btree (make-phash 'test-phash)) (phash-btree (make-phash 'test-phash))))
    (ele:drop-btree (phash-btree (make-phash 'test-phash)))))

(def test table-primitives ()
  (ele:ensure-transaction ()
    (with-fixture |phash|
      (dotimes (i 10)
        (map-put *phash* i (* 2 i))
        (is (eql (map-get *phash* i) (* 2 i)))))))



;; (object-identity)
;; (table-primitives)
