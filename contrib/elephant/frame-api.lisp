;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;;

(in-package :templeton)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WITH-* Bridge Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (with-macro* e) with-graph (node)
  (let* ((w:*db* (named-graph-db node))
          (result (-with-macro/body-)))
    (values result *db* node)))

(def (with-macro* e) with-merged-graphs (&rest nodes)
  (let ((*db* (make-instance 'extended-db :emptyp t)))
    (dolist (node (alexandria:ensure-list nodes))
      (db-merge *db* node))
    (let ((result (-with-macro/body-)))
    (values result *db* nodes))))

(def (with-macro e) with (graph)
  (with-graph (graph)
    (-with-macro/body-)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Named-Graph Frame API extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-frame (graph node &rest slot/value-pairs)
  (with-graph (graph)
    (apply #'frame (push node slot/value-pairs))
    node))

;; (make-frame !rdfs: !rdf:nil  '(!rdfs:comment . #"framey") '(!rdfs:label . #"neato"))

(defun frame-slots (graph node)
  (with-graph (graph)
    (own-slots node)))

(defun frame-value (graph node &rest paths)
  (with-graph (graph)
    (value node paths)))

(defun frame-values (graph node &rest paths)
  (with-graph (graph)
    (all-values node paths)))

(defun frame-put (graph node slot value)
  (with-graph (graph)
    (add-value node slot value)))

(defun frame-drop (graph node slot &optional value)
  (with-graph (graph)
    (del-value node slot value)))

(defun frame-related-p (graph src path sink &optional action)
  (with-graph (graph)
    (relatedp src path sink action)))

(defun frame-cbd (graph node)
  (db-find-cbd (named-graph-db graph) node))

(defun cbd (major &optional minor)
  (if minor
    (frame-cbd major minor)
    (db-find-cbd *db* major)))
  
#|

 (with-merged-graphs (!rdf: !skos:)
   (pprint (own-slots !rdf:Statement))
   (describe *db*)) 
 (!rdf:type !rdfs:isDefinedBy !rdfs:label !rdfs:subClassOf !rdfs:comment)
|#
