;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;


(in-package :templeton)


(let ((db-api '(w::db-add-namespace w::db-node-resolved w::db-triples w::db-add-triple 
                 w::db-find-triple w::db-make-triple w::db-count-triples w::db-path-fsas w::db-clear
                 w::db-merge w::db-query w::db-query-by-source w::db-sources w::db-source-descs
                 w::db-del-source w::db-literal-class w::db-new-container-membership-property
                 w::db-make-literal w::owl-list w::owl-parser w::owl-cons w::db-access-stats
                 w::db-index-stats w::db-del-triple w::db-find-cbd
                 )))
  (mapc #'import db-api)
  (export db-api *package*))

(defmethod w::db-find-cbd ((db node) node)
  (w::db-find-cbd (named-graph-db db) node))

(defmethod w::db-access-stats ((db node))
  (w::db-access-stats (named-graph-db db)))

(defmethod w::db-index-stats ((db node))
  (w::db-index-stats (named-graph-db db)))

(defmethod w::db-add-namespace ((db node) prefix uri)
  (w::db-add-namespace (named-graph-db db) prefix uri))

(defmethod w::db-node-resolved ((db node) node old-name)
  (w::db-node-resolved (named-graph-db db) node old-name))

(defmethod w::db-triples ((db node))
  (w::db-triples (named-graph-db db)))

(defmethod w::db-add-triple ((db node) triple &optional source)
  (w::db-add-triple (named-graph-db db) triple source))
  
(defmethod w::db-del-triple ((db node) triple &optional source)
  (w::db-del-triple (named-graph-db db) triple source))
  
(defmethod w::db-find-triple ((db node) triple)
  (w::db-find-triple (named-graph-db db) triple))

(defmethod w::db-make-triple ((db node) subject predicate object &optional source)
  (w::db-make-triple (named-graph-db db) subject predicate object source))

(defmethod w::db-count-triples ((db node))
  (w::db-count-triples (named-graph-db db)))

(defmethod w::db-path-fsas ((db node))
  (w::db-path-fsas (named-graph-db db)))

(defmethod w::db-clear ((db node))
  (w::db-clear (named-graph-db db)))

(defmethod w::db-merge ((to node) from &optional source)
  (w::db-merge (named-graph-db to) from source)
  (commit (named-graph to)))

(defmethod w::db-merge (to (from node) &optional source)
  (w::db-merge to (named-graph-db from) source))

(defmethod w::db-query ((db node) subject predicate object)
  (w::db-query (named-graph-db db) subject predicate object))

(defmethod w::db-query-by-source ((db node) source)
  (w::db-query-by-source (named-graph-db db) source))

(defmethod w::db-sources ((db node))
  (w::db-sources (named-graph-db db)))

(defmethod w::db-source-descs ((db node))
  (w::db-source-descs (named-graph-db db)))

(defmethod w::db-del-source ((db node) source)
  (w::db-del-source (named-graph-db db) source))

(defmethod w::db-literal-class ((db node))
  (w::db-literal-class (named-graph-db db)))

(defmethod w::db-new-container-membership-property ((db node) property)
  (w::db-new-container-membership-property (named-graph-db db) property))

(defmethod w::db-make-literal ((db node) string &rest options &key property datatype language
                                &allow-other-keys)
  (declare (ignore options))
  (apply #'w::db-make-literal
    (append (list (named-graph-db db) string) 
      (list :property property :datatype datatype :language language))))
