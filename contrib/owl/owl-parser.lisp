;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :wilbur)


(defclass owl-parser (rdf-parser)
  ())

(defmethod start-element :before ((parser owl-parser)
				  (tag open-tag)
				  (mode (eql :property)))
  (when (string= (token-string tag) -owl-imports-uri-)
    (ensure-ontology-loaded parser (tag-attribute tag -rdf-resource-uri-))))

(defmethod ensure-ontology-loaded ((parser owl-parser) url)
  ;; simplistic ontology loader, can be overridden
  (cond ((db-query *db* (node url) !rdf:type !owl:Ontology)
	 (format t "owl:imports ~a already done~%" url))
	(t
	 (format t "Loading ~a~%" url)
	 (db-load *db* url :merge-results-p t))))

(defmethod parse-using-parsetype ((parser owl-parser) node property parsetype
                                  &optional statement-id language datatype)
  (declare (ignore statement-id language datatype))
  (if (not (string= parsetype "collection"))
    (call-next-method)
    (let ((list-node (ensure-node parser nil t)))
      (add-as-triple parser node property list-node)
      (add-state parser :owl-collection list-node))))

(defmethod start-element ((parser owl-parser)
			  (tag open-tag)
			  (mode (eql :owl-collection)))
  (start-element parser tag :description))

(defmethod attach-to-parent ((parser owl-parser)
                             (parent node)
                             (child node)
                             &optional statement-id)
  (declare (ignore statement-id))
  (let ((state (first (parser-states parser))))
    (if (not (eq (state-mode state) :owl-collection))
      (call-next-method)
      (let ((parent (state-node state))
            (node (ensure-node parser nil t)))
        (add-as-triple parser parent -rdf-type-uri-
                       (ensure-node parser -owl-list-uri- t))
        (add-as-triple parser parent -owl-first-uri- child)
        (add-as-triple parser parent -owl-rest-uri- node)
        (setf (state-node state) node)))))

(defmethod end-element :before ((parser owl-parser)
				(tag open-tag)
				(mode (eql :owl-collection)))
  (let* ((node (state-node (first (parser-states parser))))
         (db (parser-db parser))
         (triple (db-del-triple db (first (db-query db nil nil node)))))
    (add-as-triple parser
                   (triple-subject triple)
                   (triple-predicate triple)
                   (ensure-node parser -owl-nil-uri- t))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   CONSTRUCTORS FOR OWL COLLECTIONS
;;;

(defun owl-list (&rest items)
  (declare (dynamic-extent items))
  (if items
    (owl-cons (first items) (apply #'owl-list (rest items)))
    !owl:nil))

(defun owl-cons (first rest &optional uri)
  (let ((pair (node uri)))
    (add-triple (triple pair !rdf:type (node -owl-list-uri-)))
    (add-triple (triple pair (node -owl-first-uri-) first))
    (add-triple (triple pair (node -owl-rest-uri-) rest))
    pair))

;; (export '(owl-list owl-parser))
