;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;;

(in-package :wilbur)

(defclass storable-graph-container (graph-container)
  (name container-classname
    (last-state :initarg :last-state :accessor last-state-of)
    (db :initarg db :transient t :accessor db-of))
  (:metaclass ele:persistent-metaclass))

(defmethod (setf db-of) :after ((db wilbur:db) (container storable-graph-container))
  (setf (last-state-of container) db))

(defmethod initialize-instance :after ((container storable-graph-container) &rest args)
  (declare (ignore args))
  (when (slot-boundp container 'last-state)
    (setf (db-of container) (last-state-of container))))
