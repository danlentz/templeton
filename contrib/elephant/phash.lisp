;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :templeton)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHASH / PINDEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def logger phash (triple-spaced.hashing::hashing))
 
(def (class* eas) phash ()
  ((map :accessor phash-map))
  (:default-initargs :map (ele:make-btree)))

(def (function e) make-phash (&optional (name (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root name)))
    (if btree
      (make-instance 'phash :map btree)
      (let ((phash (make-instance 'phash)))
        (ele:add-to-root name (phash-map phash))
        phash))))

(def (class* eas) pindex (phash)
  ((map :accessor pindex-map))
  (:default-initargs :map (ele:make-indexed-btree)))

(def (function e) make-pindex (&optional (name (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root name)))
    (if btree
      (make-instance 'pindex :map btree)
      (let ((pindex (make-instance 'pindex)))
        (ele:add-to-root name (pindex-map pindex))
        pindex))))


(def (function e) phash-help ()
  " (describe-btree (phash-map  (make-phash 'test-phash)))")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexer Protocol: PHASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def method map-get ((phash phash) key)
  (ele:ensure-transaction ()
    (ele:get-value key (phash-map phash))))

(def (function ed) get-phash (key phash)
  (map-get phash key))

(def method map-put ((phash phash) key value)
  (ele:ensure-transaction ()
    (setf (ele:get-value key (phash-map phash)) value)))

(def (function ed) put-phash (key value phash)
  (map-put phash key value))

(def method map-ensure-get ((phash phash) key otherwise-put)
  (ele:ensure-transaction ()
    (or
      (values (map-get phash key) t)
      (values (map-put phash key otherwise-put) nil))))


(def (function ed) get-phash* (key phash otherwise-put)
  (map-ensure-get phash key otherwise-put))

(defsetf map-get map-put)
(defsetf get-phash put-phash)

(def method map-drop ((phash phash) key)
  (ele:ensure-transaction ()
    (remove-kv key (phash-map phash))))

(def (function ed) rem-phash (key phash)
  (map-drop phash key))

(def method map-size ((phash phash))
  (let (count)
    (ele:ensure-transaction ()
      (map-btree (lambda (k v)
                   (declare (ignore k v))
                   (incf count))
        (phash-map phash))
      count)))

(def (function ed) phash-size (phash)
  (map-size phash))
  
(def method map-p ((phash phash))
  (map-p (phash-map phash)))

(def method map-p ((btree ele:btree))
  t)

(def method map-map  ((phash phash) fn/2)
  (ele:ensure-transaction ()
    (ele:map-btree fn/2 (phash-map phash))))

(def (function ed) map-phash (fn phash)
  (map-map phash fn))

;; (def (function ed) clear-phash (phash)
  ;;(setf (phash-map phash) (ele:make-btree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def suite* (test :in root-suite))

(def special-variable *phash*)

(def fixture |phash|
  (unwind-protect
    (let ((*phash*  (make-phash 'test-phash)))
      (-body-))
    (ele:drop-btree (phash-map (make-phash 'test-phash)))))

(def test object-identity ()
  (with-fixture |phash|
    (is (not (eq (make-phash 'test-phash) (make-phash 'test-phash))))
    (is (eq (phash-map (make-phash 'test-phash)) (phash-map (make-phash 'test-phash))))
    (ele:drop-btree (phash-map (make-phash 'test-phash)))))

(def test table-primitives ()
  (ele:ensure-transaction ()
    (with-fixture |phash|
      (dotimes (i 10)
        (map-put *phash* i (* 2 i))
        (is (eql (map-get *phash* i) (* 2 i)))))))



;; (object-identity)
;; (table-primitives)
