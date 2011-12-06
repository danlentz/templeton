 ;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;;

(in-package :templeton)

(defclass layered-node (w:node id:contextual-object-uuid)
  ())


(defmethod initialize-instance :after ((node layered-node) &key base-namespace)
  (id::update-contextual-object-uuid node
    :base-namespace (or base-namespace (context)) :control-id (node-uri node)))


(defmethod (setf node-uri) :after ((uri string) (node storable-node))
  (id::update-contextual-object-uuid node
    :base-namespace (id:contextual-identity-parent-uuid node) :control-id node))


(defclass layered-context-dictionary (dictionary)
  ((context :initarg :base-namespace :initarg :context :accessor dictionary-context))
  (:default-initargs :node-class 'layered-node :context (context)))

(defun dictionary-clear ()
  (setf *nodes* (make-instance 'layered-context-dictionary)))

(defun dictionary-size (&optional (dict *nodes*))
  (hash-table-size (slot-value dict 'w::nodes)))

(defvar *dictionary* (dictionary-clear))


;; (defmethod initialize-instance :after ((dict storable-node-dictionary) &rest args)
;;   (declare (ignore args))
;;   (dictionary-add-namespace dict *INSTANCE-PREFIX* *INSTANCE-URI*)
;;   (let ((nodes
;;           (or (slot-value dict 'w::nodes)
;;             (setf (slot-value dict 'w::nodes) (make-hash-table :test 'equal)))))
;;     (map-class #'(lambda (x) (setf (gethash (node-uri x) nodes) x))
;;       (find-class 'storable-node))
;;     (printv nodes)))
;; (when (and ele:*store-controller* (not (typep *nodes* 'storable-node-dictionary)))
;;   (dictionary-clear))

