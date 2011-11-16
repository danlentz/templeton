;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; put together some named-graph api combining elephant and wilbur with efficently
;;;;; persistent storage of indexed graphs that presents a 'graph prevalence' storage
;;;;; model.  More specifics on how this is accomplished can be found in a tutorial
;;;;; on the design pattern by Ian Eislick Elephant 1.0 Reference manual section 6.3
;;;;; see the actual storage implementation in snapshot.lisp


(in-package :templeton)

(defun all-prefixes ()
  (w:dictionary-namespaces w:*nodes*))

(defvar *graphs* (make-hash-table :test 'equal))

(defclass extended-db (w::edb ttl:turtle-db
                        w::db-access-counter-mixin
                        w::indexed-literal-db-mixin
                        w::date-cleanup-db-mixin)
  ()
  (:default-initargs :index-literals-p t :blank-node-uri-prefix "_:"))
  
(unless w:*db*
  (setf w:*db* (make-instance 'extended-db)))

(defclass graph-container ()
  ())

(defclass named-graph-db (w::interned-literal-indexed-db                      
                           w::literal-transform-db-mixin
                           ttl:turtle-db
                          w::db-access-counter-mixin)
  ()
  (:default-initargs :emptyp t :blank-node-uri-prefix "_:"))

(defgeneric find-graph (identifier &optional errorp))
(defgeneric put-graph (container identifier))

(defclass storable-graph (w:node graph-container snapshot-set)
  ((wilbur::uri :index t))
  (:metaclass ele:persistent-metaclass))

(defgeneric db-of (graph-designator))

(defmethod (setf db-of) ((db wilbur:db) (container storable-graph))
;;  (register-object db container)
  (setf (snapshot-root container) db)
  (snapshot-commit container)
  )

#+()
(defmethod (setf db-of) :after ((db wilbur:db) (container storable-graph))
  nil)

;;  (snapshot-commit container))

(defmethod db-of ((container storable-graph))
  (snapshot-root container))
                   
(defmethod initialize-instance :after ((container storable-graph) &rest args)
  (declare (ignore args))
  (unless (db-of container)
    (setf (db-of container) (make-instance 'named-graph-db))))

(defmethod find-graph ((identifier string) &optional errorp)
  (multiple-value-bind (value foundp) (gethash identifier *graphs*)
    (if foundp
      value
      (multiple-value-bind (value foundp) (ele:get-from-root identifier)
        (if foundp
          (prog1 value
            (snapshot-restore value))
          (if errorp
            (error "~A not found" identifier)
            nil))))))

(defmethod find-graph ((identifier w:node) &optional errorp)
  (find-graph (w:node-uri identifier) errorp))

(defmethod put-graph (container (identifier string))
  (ele:ensure-transaction  ()
    (setf (w:node-uri container) identifier)
    (ele:add-to-root identifier container)
    (setf (gethash identifier *graphs*) container)))

(defsetf find-graph put-graph)

(defun make-graph (id &optional (classname 'storable-graph))
  (put-graph (make-instance classname) id))

(defgeneric named-graph (identifier))

(defmethod named-graph ((id string))  
  (or
    (find-graph id)
    (make-graph id)))

(defmethod named-graph ((id w:node))
  (named-graph (w:node-uri id)))

(defmethod named-graph ((id storable-graph))
  id)

(defun ng (id)
  (named-graph id))

(defun named-graph-db (id)
  (db-of (named-graph id)))

(defun ndb (identifier)
  (db-of (named-graph identifier)))

(defgeneric commit-graph (graph-designator))

(defmethod commit-graph ((g storable-graph))
  (snapshot-commit g))

(defmethod commit-graph ((id string))
  (snapshot-commit (named-graph id)))

(defmethod commit-graph ((id w:node))
  (snapshot-commit (named-graph (w:node-uri id))))

(defun commit (id)
  (commit-graph id))


(defgeneric rollback-graph (graph-designator))

(defmethod rollback-graph ((g storable-graph))
  (snapshot-restore g))

(defmethod rollback-graph ((id string))
  (snapshot-restore (named-graph id)))

(defmethod rollback-graph ((id w:node))
  (snapshot-restore (named-graph (w:node-uri id))))

(defun rollback (id)
  (rollback-graph id))

(defun all (type) 
  (let (result)
    (ele:map-root #'(lambda (k v) 
                      (when (eq (class-of v)
                              (find-class type))
                        (push (cons k v) result)))) 
    result))

(defun all-storable-graphs ()
  (all 'storable-graph))

(defun all-named-graphs ()
  (all-storable-graphs))

(defun drop-all-storable-graphs ()
  (let ((graphs  (mapcar #'car (all-storable-graphs))))
    (mapc #'ele:remove-from-root graphs)
    (mapcar #'(lambda (symbol)
              (remhash symbol *graphs*))
      graphs)))

(defun drop-all-named-graphs ()
  (drop-all-storable-graphs))
  
(defgeneric drop-graph (id))

(defmethod drop-graph ((id string))
  (ele:ensure-transaction ()
    (ele:remove-from-root id))
  (remhash id *graphs*))

(defmethod drop-graph ((id w:node))
  (drop-graph (w:node-uri id)))
  
(defgeneric import-graph (src dst))

(defmethod import-graph ((uri puri:uri) (id string))
                 (let ((g (named-graph id)))
                   (w:db-load (db-of g) (princ-to-string uri) :appendp t :merge-results-p t)
                   (snapshot-commit g)
                   (setf (symbol-value (intern id "_")) g)
                   (export (intern id "_") "_")
                   g))

(defun load-schema (uri-designator)
  (typecase uri-designator
    (puri:uri (import-graph uri-designator (princ-to-string uri-designator)))
    (string (import-graph (puri:uri uri-designator) uri-designator))
    (node (load-schema (node-uri uri-designator)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move to setf specific module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defgeneric graph-mediator (identifier &optional class-name))

(export 'graph-mediator)


;(defclass storable-graph-mediator (wilbur-mediator)) 
(defmethod graph-mediator ((identifier null) &optional (class 'rdf::wilbur-mediator))
  (funcall class :db w:*db*))
|#
#|
(graph-mediator ())
#<DE.SETF.RESOURCE:WILBUR-MEDIATOR mediating:
   #<EDB size 69 {1004243851}> x #<NON-TRANSACTIONAL {100409D641}> {1005CFD061}>
|#
#|
(defmethod graph-mediator ((identifier symbol) &optional (class 'rdf::wilbur-mediator))
  (funcall class :db (db-of (ensure-graph identifier))))

(defmethod graph-mediator ((identifier w:db) &optional (class 'rdf::wilbur-mediator))
  (funcall class :db identifier))
|#
;; (graph-mediator 'x)
;;#<DE.SETF.RESOURCE:WILBUR-MEDIATOR mediating:
;;  #<INDEXED-DB size 78 {100F6D6241}> x #<NON-TRANSACTIONAL {100409D641}> {10091FCB21}>

;; (graph-mediator 'x)
;; #<DE.SETF.RESOURCE:WILBUR-MEDIATOR mediating:
;;   #<INDEXED-DB size 78 {100F6D6241}> x #<NON-TRANSACTIONAL  {100409D641}> {10091D1821}>


;; (import 'RDF:project-graph)
#|

;; (defmethod project-graph ((src w:db)(dst w:db))
;;   (w:db-merge dst src)
;;   dst)

;; (defmethod project-graph ((src RDF:repository-mediator)(dst RDF:repository-mediator))
;;   (project-graph (RDF:mediator-repository src) (RDF:mediator-repository dst)))

;; (defmethod project-graph ((src symbol)(dst symbol))
;;   (project-graph (graph-mediator src) (graph-mediator dst)))

;; (defmethod project-graph ((src symbol) dst)
;;   (project-graph (graph-mediator src) dst))

;; (defmethod project-graph (src (dst symbol))

;;   (project-graph src (graph-mediator dst))
;;   (ele:add-to-root dst (ensure-graph dst)) 
;;   )

;; (defun all-storable-graphs (&optional (store web:*default-store*))
;;   (web:find-persistent-objects store 'storable-graph-container))

;; (all-storable-graphs);;
;; (#<STORABLE-GRAPH-CONTAINER oid:302> #<STORABLE-GRAPH-CONTAINER oid:303>)
|#



