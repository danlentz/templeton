;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; Wilbur does not explicitly specify its three core protocols (db, dictionary, source)
;;;;; but rather lets them be defined implicitly through implementation of concrete 'defmethods'.
;;;;; we provide defgeneric here in order to serve as a reference to the abstract protocol
;;;;; to facilitate our work on implementation of conforming extensions.
;;;;;

(in-package :wilbur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core DB Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric db-add-namespace (db prefix uri))

(defgeneric db-node-resolved (db node old-name))

(defgeneric db-triples (db))

(defgeneric db-add-triple (db triple &optional source))

(defgeneric db-del-triple (db triple &optional source))

(defgeneric db-find-triple (db triple))

(defgeneric db-make-triple (db subject predicate object &optional source))

(defgeneric db-count-triples (db))

(defgeneric db-path-fsas (db))

(defgeneric db-clear (db))

(defgeneric db-merge (to from &optional source))

(defgeneric db-query (db subject predicate obect))

(defgeneric db-query-by-source (db source))

(defgeneric db-sources (db))

(defgeneric db-source-descs (db))

(defgeneric db-del-source (db source))

(defgeneric db-literal-class (db))

(defgeneric db-new-container-membership-property (db property))

(defgeneric db-make-literal (db string &rest options &key property datatype language
                              &allow-other-keys ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexed-DB Protocol Extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric db-by-source (indexed-db))

(defgeneric db-index-s (indexed-db))

(defgeneric db-index-p (indexed-db))

(defgeneric db-index-o (indexed-db))

(defgeneric db-index-po (indexed-db))

(defgeneric db-index-sp (indexed-db))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionary Protocol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dictionary-add-namespace (dictionary prefix uri))

(defgeneric dictionary-apropos-list (dictionary pattern))

(defgeneric dictionary-make-node (dictionary uri))

(defgeneric dictionary-namespaces (dictionary))

(defgeneric dictionary-node-class (dictionary))

(defgeneric dictionary-nodes (dictionary))

(defgeneric dictionary-remove-namespace (dictionary prefix))

(defgeneric dictionary-rename-namespace (dictionary old-prefix new-prefix))

(defgeneric dictionary-unresolved-nodes (dictionary))

(defgeneric find-long-name (dictionary name))

(defgeneric find-short-name (dictionary uri &optional use-entities-p))

(defgeneric find-unresolved-nodes (dictionary))

(defgeneric find-node (dictionary uri))
