;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; still somewhat wilbur-oriented target production
;;;;;

(in-package   :gs.ebu.wilbur.turtle)

(in-readtable extended)

(defclass turtle-db (wilbur:blank-node-db-mixin wilbur:db)
  ((wilbur::blank-node-uri-prefix :initform "_:")
    (bnodes :reader bnodes-of :initform (make-hash-table :test 'equal)))
  (:documentation "un-indexed temporary db not suitable for general purpose use"))


(defun intern-bnode (db id)
  (let* ((bnodes (bnodes-of db))
          (bnode  (gethash id bnodes)))
    (or bnode  (let ((b (wilbur:node nil)))
                 (setf (gethash id bnodes) b)
                 b))))


(defun intern-in-db (db ast-thing)
  (ecase (first ast-thing)

    ((:BLANK)
      (wilbur:node nil))

    ((:NODEID)
      (intern-bnode db (second ast-thing)))

    ((:QNAME)
      (wilbur::unresolved-node (second ast-thing)))

    ((:BOOLEAN)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:boolean))
    ((:DECIMAL)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:decimal))
    ((:DOUBLE)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:double))
    ((:INTEGER)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:integer))
    ((:STRING)
       (wilbur::db-make-literal db (second ast-thing)
                                :datatype !xsd:string
                                :language (third ast-thing)))    
    ((:URIREF)
      (let ((uriref (second ast-thing)))
        (wilbur::node (subseq uriref 1 (- (length uriref) 1)))))))


(defun turtle-ast-to-rdf-db (turtle-ast)
  "Create a Wilbur store and populate with TURTLE-AST."
  (let* ((db (make-instance 'turtle-db))
          (wilbur:*db* db))
    (loop for statement in (rest turtle-ast)
      do (logv:logv (ecase (first statement)
                      ((:@prefix)
                        (let ((prefix (subseq (second statement)
                                        0 (- (length (second statement)) 1))))
                          (wilbur:add-namespace prefix (third statement))))
                      ((:@base)
                        (error "@base unimplemented"))
                      ((:triples)
                        (parse-triples db statement)))))
    db))


(defun parse-resource (db statement)
  (case (first statement)
    ((:blank)
      (let ((blank (intern-in-db db statement)))
        (parse-triples-aux db blank (rest statement))
        blank))
    ((:collection)
      (parse-collection db statement))
    (t
      (intern-in-db db statement))))


(defun parse-collection (db collection)
  (labels ((aux (collection)
             (if collection
               (let* ((tail (aux (rest collection)))
                       (head (parse-resource db (first collection)))
                       (cons (wilbur:node nil)))
                 (wilbur:db-add-triple db (wilbur:db-make-triple db cons !rdf:head head))
                 (wilbur:db-add-triple db (wilbur:db-make-triple db cons !rdf:rest tail))
                 cons)
               !rdf:nil)))
    (aux (rest collection))))



(defun parse-triples (db statement)
  (parse-triples-aux db (parse-resource db (second statement))
                     (cddr statement)))


(defun parse-triples-aux (db subject predicate-object-lists)
  (loop for predicate-object-list in predicate-object-lists
    do (let ((predicate (intern-in-db db (second predicate-object-list))))
         (loop for object in (rest (third predicate-object-list))
           do (let* ((object (parse-resource db object))
                      (triple (wilbur:db-make-triple db subject predicate object)))
                (wilbur:db-add-triple db triple))))))

