;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :templeton)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHASH / PINDEX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *context* nil)

(defun context (&optional uuid)
  (or *context*
    (if uuid (setf *context* uuid)
      (setf *context* (unicly:make-v4-uuid)))))
  
(defun arc (uuid)
  (unicly:uuid-bit-vector-to-byte-array
    (unicly:uuid-to-bit-vector (id:contextual-identity-uuid uuid))))


(define-symbol-macro |<>| (context))


(def (class* eas) phash (simple-print-object-mixin)
  ((btree :initarg :btree :accessor phash-btree))
  (:default-initargs :btree (ele:make-btree)))

(def (function e) make-phash (&optional (uuid (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root (princ-to-string uuid))))
    (if btree
      (make-instance 'phash :btree btree)
      (let ((phash (make-instance 'phash)))
        (ele:add-to-root (princ-to-string uuid) (phash-btree phash))
        phash))))

(def (class* eas) pindex (phash simple-print-object-mixin)
  ((btree :accessor pindex-btree))
  (:default-initargs :btree (ele:make-indexed-btree)))

(def (function e) make-pindex (&optional (uuid (unicly:make-v4-uuid)))
  (let ((btree (ele:get-from-root (princ-to-string uuid))))
    (if btree (make-instance 'pindex :btree btree)
      (let ((pindex (make-instance 'pindex)))
        (ele:add-to-root (princ-to-string uuid) (pindex-btree pindex))
        pindex))))

(def (class* eas) ptrie (pindex layered-node)
  ((trie :initarg :trie  :accessor ptrie-trie :initform (make-trie))))



(def (function e) make-ptrie (&optional (uuid (context)))
  (make-instance 'ptrie :btree (pindex-btree (make-pindex uuid))
    :uri (unicly:uuid-as-urn-string nil uuid)))

(def (function e) ptrie (context identifier)
  (typecase identifier
    (w:node
      (ptrie context (w:node-uri identifier)))
    (string
      (let ((ptrie (make-instance 'ptrie :btree (pindex-btree (make-pindex (unicly:make-v5-uuid context identifier))) :uri identifier)))
        (set-trie-value (ptrie-trie ptrie) (node identifier))
        (setf (get-trie (arc (make-null-uuid)) (ptrie-trie ptrie)) context)
        ptrie))
    (null (make-ptrie (or context (context))))))

(defmethod print-object ((ptrie ptrie) stream)
  (print-object (w:node (w:node-uri ptrie)) stream))


(defun make-context (&optional (node *context*))
  (let ((*context* node)
         (*root* (make-trie)))
    (lambda (property &optional value)
      (cond
        ((and (not property) (not value)) *root*)
        ((and (eql property t) (eql value t))
          (maptrie #'(lambda (x y) (:printv x y)) *root*))
        ((and (eql property t) (not value)) *context*)
        ((and (eql property t) (typep value 'w:node))
          (setf
            *context* (id:contextual-identity-uuid value)
            *root*    (make-trie)))
        ((and property (not value)) (get-trie (arc property) *root*))
        ((and property value) (setf (get-trie (arc property) *root*) value))
        (t nil)))))

(defun contextv ()
  (maptrie #'(lambda (x y) (:printv x y))  (funcall x nil)))

(describe
(make-context))
;;(defmacro defcontext 
  ;; (typecase control-id
  ;;   (w:node (make-ptrie base-namespace (w:node-uri control-id)))
  ;;   (t
  ;;     (let* (#+()(uuid (id:make-context-object-uuid
  ;;                    :base-namespace base-namespace
  ;;                    :control-id control-id))
  ;;             (phash (make-phash (unicly:make-v5-uuid base-namespace control-id)))
  ;;             (trie (new-root-hybrid-trie nil nil))
  ;;             (ptrie (make-instance 'ptrie  :trie trie :btree (phash-btree phash) )))
  ;;       (setf (ptrie-uuid ptrie)  (unicly:make-v5-uuid base-namespace control-id))
  ;;       (setf (root-trie-purpose trie) control-id)
  ;;       (setf (node-uri ptrie) control-id)
  ;;       (setf (root-trie-up trie) base-namespace)
  ;;       ptrie))))
    
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
  (let ((count 0))
    (ele:ensure-transaction ()
      (map-btree #'(lambda (k v)
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

(def suite* (elephant :in test))
(in-suite elephant)

(def special-variable *phash*)

(def fixture |phash|
  (elephant:ensure-transaction ()
    (unwind-protect (let ((*phash*  (make-phash 'test-phash)))
                      (-body-))
      (progn 
        (ele:drop-btree (phash-btree (make-phash 'test-phash)))
        (setf *phash* nil)))))


(def test phash-representational-identity.0 ()
  (with-fixture |phash|
    (is (not (eq
               (make-phash 'test-phash)
               (make-phash 'test-phash))))
    (is (eq
          (phash-btree (make-phash 'test-phash))
          (phash-btree (make-phash 'test-phash))))))


(def test phash-table-primitives.0 (&optional (iterations 100))
  (ele:ensure-transaction ()
    (with-fixture |phash|
      (dotimes (i iterations)
        (phash-put i (* 2 i) *phash*))
      (dotimes (i iterations)
        (is (eql (phash-get i *phash*) (* 2 i)))))))


(def test phash-operational-persistence.0 (&optional test-subject-population &aux (count 0))
  (if test-subject-population (setf count (length test-subject-population))
    (do-external-symbols (sym (find-package :common-lisp))
      (unless (eq sym 'nil) ;; exclude nil
        (incf count)
        (push sym test-subject-population))))  
  (unless (every #'identity test-subject-population)
    (break/inspect test-subject-population))
  (with-fixture |phash|
    (map nil #'(lambda (subject) (phash-put (write-to-string subject) subject *phash*))
      test-subject-population)
    (is (equal (phash-size *phash*) count))
    (setf *phash* nil)    
    (is (setf *phash* (make-phash 'test-phash)))
    (is (equal (phash-size *phash*) count))
    (is (null  (set-difference test-subject-population 
                 (mapcar
                   #'(lambda (subject &aux restored-original-value)
                       (setf restored-original-value (phash-get (write-to-string subject) *phash*))
                       (is restored-original-value)
                       (is (find restored-original-value test-subject-population)) ;; :test #'eq))
                       restored-original-value)
                   test-subject-population))))))
