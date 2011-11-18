;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;; database and index persistence implementation based on an alternative 
;;; transaction model that provides atomic commit semantics and durable
;;; point-in-time recovery of all mutable domain-model content and persistent
;;; state. In addition, performance characteristics of this approach are
;;; comparible to those of the current non-persistent model.
;;;
;;; Adapted from snapshot/changeset implementation 
;;; Copyright (c) 2007 Ian Eslick <ieslick common-lisp net>
;;;
;;;



(in-package :templeton)

(defclass snapshot-set ()
  ((index
     :initform (ele:make-btree)
     :accessor snapshot-set-index)
    (next-id
      :initform 0
      :accessor snapshot-set-next-id)
    (root
      :initform nil
      :accessor snapshot-set-root)
    (cache :transient t
      :initform (make-hash-table :weakness :value)
      :accessor snapshot-set-cache) 
    (touched :transient t
      :initform (make-array 20 :element-type 'fixnum :initial-element 0
                  :fill-pointer t :adjustable t)
      :accessor snapshot-set-touched))       
  (:metaclass ele:persistent-metaclass)
  (:documentation "Keeps track of a set of standard objects allowing a
  single snapshot call to update the store controller with the latest
  state of all objects registered with this set"))

(defmethod initialize-instance :after ((set snapshot-set) &key lazy-load &allow-other-keys)
  (unless lazy-load (snapshot-restore set)))

;; new
(defmethod describe-object :after ((o snapshot-set) stream)
  (declare (ignore stream))
  (terpri)
  (format t "Root Object:~%~A~%~%" (snapshot-root o))
  (btree-info (snapshot-set-index o))
  (terpri))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod register-object ((object standard-object) (set snapshot-set))
  "Register a standard object.  Not recorded until commit is called on db"
  (ele::aif (lookup-cached-id object set)
    (values object ele::it)
    (let ((id (incf (snapshot-set-next-id set))))
      (cache-snapshot-object id object set)
      (values object id))))


(defmethod register-object ((hash hash-table) (set snapshot-set))
  "Adds a hash table to the snapshot set and registers any standard objects
   stored as values that are not already part of the snapshot.  Must call commit
   to persist."
  (ele::aif (lookup-cached-id hash set)
    (values hash ele::it)
    (let ((id (incf (snapshot-set-next-id set))))
      (cache-snapshot-object id hash set)
      (values hash id))))


;; new

(defmethod register-object ((object structure-object) (set snapshot-set))
  "Register a struct.  Not recorded until commit is called on db"
  (ele::aif (lookup-cached-id object set)
    (values object ele::it)
    (let ((id (incf (snapshot-set-next-id set))))
      (cache-snapshot-object id object set)
      (values object id))))


(defmethod register-object ((default t) (set snapshot-set))
  (error "Cannot register objects of type ~A" (type-of default)))


(defmethod unregister-object (object (set snapshot-set))
  "Drops the object from the cache and backing store"
  (let ((id (gethash object (snapshot-set-cache set))))
    (when (null id) (error "Object ~A not registered in ~A" object set))
    (drop-cached-object object set)
    (delete-snapshot-object id set)))


(defmethod snapshot-root ((set snapshot-set))
  "Get the snapshot root object"
  (when (snapshot-set-root set)
    (lookup-cached-object (snapshot-set-root set) set)))


(defmethod (setf snapshot-root) (value (set snapshot-set))
  "Specify a root object for the set.  There is only 1 so it should be
   a hash or the root node of a graph"
  (setf (snapshot-set-root set)
    (multiple-value-bind (obj id) (register-object value set)
      (declare (ignore obj)) id))
  value)


(defun map-set (fn set)
  "Iterates through all values in the active set, not the
   saved snapshot"
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (funcall fn k))
    (snapshot-set-cache set)))


(defmethod snapshot-commit ((set snapshot-set))
  "Saves all objects in the set (and any objects reachable from the
   current set of objects) to the persistent store"
  (ele:with-transaction (:store-controller (ele::get-con (snapshot-set-index set)))
    (loop for (obj . id) in (get-cache-entries (snapshot-set-cache set)) do
	  (save-snapshot-object id obj set))
    (collect-untouched set))
  (values set t))


(defmethod snapshot-restore ((set snapshot-set))
  "Snapshot-Restores a snapshot by setting the snapshot-set state to the last snapshot.
   If this is used during runtime, the user needs to drop all references
   to objects and retrieve again from the snapshot set.  Also used to initialize
   the set state when a set is created, for example pulled from the root of a
   store-controller, unless :lazy-load is specified"
  (clear-cache set)
  (ele:map-btree (lambda (id object) (load-snapshot-object id object set))
    (snapshot-set-index set))
  (values set t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cache 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun clear-cache (set)
  (clrhash (snapshot-set-cache set)))

(defun cache-snapshot-object (id obj set)
  (setf (gethash obj (snapshot-set-cache set)) id))

(defun lookup-cached-id (obj set)
  (gethash obj (snapshot-set-cache set)))

(defun find-hash-key-by-value (value hash)
  (maphash (lambda (k v)
	     (when (eq v value) 
	       (return-from find-hash-key-by-value k)))
    hash))

(defun lookup-cached-object (id set)
  (find-hash-key-by-value id (snapshot-set-cache set)))

(defun drop-cached-object (obj set)
  (remhash obj (snapshot-set-cache set)))

(defun get-cache-entries (hash)
  (let ((result nil))
    (maphash (lambda (obj id) (push (cons obj id) result))
      hash)
    result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistence Operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass setref ()
  ((id :accessor snapshot-set-reference-id :initarg :id)))

(defun setrefp (obj)
  (eq (type-of obj) 'setref))

(defun standard-object-subclass-p (obj)
  (subtypep (type-of obj) 'standard-object))

(defun structure-object-subtype-p (obj)
  (subtypep (type-of obj) 'structure-object))

(defun touch (id set)
  (vector-push-extend id (snapshot-set-touched set) 50))

(defun touched (id set)
  (find id (snapshot-set-touched set)))

(defun clear-touched (set)
  (loop for i fixnum from 0 upto (1- (length (snapshot-set-touched set))) do
    (setf (aref (snapshot-set-touched set) i) 0)))


(defun save-snapshot-object (id obj set)
  (unless (touched id set)
    (touch id set)
    (setf (ele:get-value id (snapshot-set-index set))
      (cond
        ((standard-object-subclass-p obj) (save-proxy-object obj set))
        ((structure-object-subtype-p obj) (save-proxy-struct obj set))
        ((hash-table-p obj)               (save-proxy-hash obj set))
        (t (error "Can only snapshot standard-objects and hash-tables")))))
  id)


(defun save-proxy-object (obj set)
  (printv :ff "standard-object" obj)
  (let ((svs (ele::subsets 2 (ele::slots-and-values obj))))
    (if (some #'reify-class-p (mapcar #'second svs))
      (let ((proxy (make-instance (type-of obj))))
        (loop for (slotname value) in svs do
          (setf (slot-value proxy slotname)
            (if (reify-class-p value)
              (reify-value value set)
              value)))
        proxy)
      obj)))

(defun save-proxy-struct (obj set)
  (printv :ff "struct" obj)
  (if (hash-table-p obj)
    (printv :hr (save-proxy-hash obj set))
 ;;   (printv
      (let ((svs (ele::subsets 2 (ele::struct-slots-and-values obj))))
        (if (some #'reify-class-p (mapcar #'second svs))
          (let ((proxy (funcall (ele::struct-constructor (type-of obj)))))
            (loop for (slotname value) in svs do
              (setf (slot-value proxy slotname)
                (if (reify-class-p value)
                  (reify-value value set)
                  value)))
            proxy)
          obj))))
;;)          
      

(defun save-proxy-hash (hash set)
  (printv :ff "hash" hash)
  (let ((proxy (make-hash-table)))
    (maphash (lambda (key value)
	       (setf (gethash key proxy)
                 (if (reify-class-p value)
                   (reify-value value set) 
                   value)))
      hash)
    proxy))


(defgeneric reify-class-p (obj)
  (:method ((obj t)) nil)
  (:method ((obj sb-thread:mutex)) nil)
  (:method ((obj standard-object))  t)
  (:method ((obj structure-object)) t)
  (:method ((obj hash-table))       t))


(defun reify-value (obj set)
  (multiple-value-bind (obj id) 
      (register-object obj set)
    (make-instance 'setref :id (save-snapshot-object id obj set))))


(defun collect-untouched (set)
  (ele:map-btree (lambda (k v) 
	       (declare (ignore v))
	       (unless (touched k set)
		 (ele:remove-kv k (snapshot-set-index set))))
	     (snapshot-set-index set))
  (clear-touched set))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recovery and Data Retrieval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-snapshot-object (id object set)
  (let ((object (ele::ifret object (ele:get-value id (snapshot-set-index set)))))
    (cond
      ((standard-object-subclass-p object) (load-proxy-object id object set))
      ((structure-object-subtype-p object) (load-proxy-struct id object set))      
      ((hash-table-p object)               (load-proxy-hash id object set))
      (t (error "Unrecognized type ~A for id ~A in set ~A" (type-of object) id set)))))


(defun load-proxy-object (id obj set)
  "create placeholder, then populate slots"
  (ele::ifret (lookup-cached-object id set)
    (progn
      (cache-snapshot-object id obj set)
      (let ((svs (ele::subsets 2 (ele::slots-and-values obj))))
        (loop for (slotname value) in svs do
          (when (setrefp value)
            (setf (slot-value obj slotname)
              (load-snapshot-object (snapshot-set-reference-id value) nil set)))))
      obj)))


(defun load-proxy-struct (id obj set)
  "create placeholder, then populate slots"
  (ele::ifret (lookup-cached-object id set)
    (progn
      (cache-snapshot-object id obj set)
      (let ((svs (ele::subsets 2 (ele::struct-slots-and-values obj))))
        (loop for (slotname value) in svs do
          (when (setrefp value)
            (setf (slot-value obj slotname)
              (load-snapshot-object (snapshot-set-reference-id value) nil set)))))
      obj)))


(defun load-proxy-hash (id hash set)
  (ele::ifret (lookup-cached-object id set)
    (progn
      (cache-snapshot-object id hash set)
      (maphash (lambda (key value)
                 (when (setrefp value)
                   (setf (gethash key hash)
                     (load-snapshot-object (snapshot-set-reference-id value) nil set))))
        hash)
      hash)))
		      

(defun delete-snapshot-object (id set)
  (ele:remove-kv id (snapshot-set-index set)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-suite test)

(defclass snapshot-test ()
  ((slot1 :accessor slot1 :initarg :slot1)
   (slot2 :accessor slot2 :initarg :slot2)))

(defun make-stest (slot1 slot2)
  (make-instance 'snapshot-test :slot1 slot1 :slot2 slot2))

(def test snapshot-set-basic-operations-test ()
 
  (let* ((set (make-instance 'snapshot-set))
	 (hash (make-hash-table))
	 (test1 (make-stest 1 2))
	 (test2 (make-stest 10 20))
	 (test3 (make-stest (make-stest 'one 'two) (make-stest 'three 'four)))
	 (test4 (make-stest (slot1 test3) (slot2 test3))))
    (loop for num from 1
          for obj in (list test1 test2 test3 test4) do
      (setf (gethash num hash) obj))
     
    (setf (snapshot-root set) hash)
    (ele:add-to-root 'snap-test-set set)
    (snapshot-commit set)
    
    ;; Clear
    (setf set nil)
    (setf hash nil)
    (elephant::flush-instance-cache ele:*store-controller*)    
    #+sbcl (cl-user::gc)

    ;; Reload
    (setf set (snapshot-restore (ele:get-from-root 'snap-test-set)))
    (is set)
    (setf hash (snapshot-root set))
    (is hash)
    (let ((t1 (gethash 1 hash))
	  (t2 (gethash 2 hash))
	  (t3 (gethash 3 hash))
	  (t4 (gethash 4 hash)))
      (is (eq 1 (slot1 t1)))
      (is (eq 20 (slot2 t2)))
      (is (eq (slot2 t3) (slot2 t4))))))



