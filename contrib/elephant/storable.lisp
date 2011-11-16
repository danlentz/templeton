;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;;

(in-package :templeton)


;; (defvar *storable-nodes* nil)
;; (get-from-root '*storable-nodes*)

(defclass storable-node (node)
  ((w::uri :transient t)
    (last-uri :initarg :last-uri :index t :accessor last-uri-of))
  (:metaclass ele:persistent-metaclass)
  (:default-initargs :uri (unicly:uuid-as-urn-string nil (unicly:make-v4-uuid))))

(defmethod (setf node-uri) :after ((uri string) (node storable-node))
  (setf (last-uri-of node) uri))

(defmethod initialize-instance :after ((node storable-node) &rest args)
  (declare (ignore args))
  (if (slot-boundp node 'last-uri)
    (setf (slot-value node 'w::uri) (last-uri-of node))
    (setf (slot-value node 'last-uri) (node-uri node))))

(defclass storable-node-dictionary (dictionary)
  ()
  (:default-initargs :node-class 'storable-node))

(defun dictionary-clear ()
  (setf *nodes* (make-instance 'storable-node-dictionary)))



(when (and ele:*store-controller* (not (typep *nodes* 'storable-node-dictionary)))
  (dictionary-clear))

