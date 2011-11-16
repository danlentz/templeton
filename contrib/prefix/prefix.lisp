;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :templeton)



(defun all-prefixes ()
  (w:dictionary-namespaces w:*nodes*))

(defvar *prefix-graph* nil)

(defun prefix-db ()
  (or (and *prefix-graph* (db-of *prefix-graph*))
    (db-of (setf *prefix-graph* (named-graph "ns")))))

(defun commit-prefix-db ()
  (commit (named-graph "ns")))
    
(defun resolve-prefix (string)
  (or (cdr (assoc string (all-prefixes) :test 'equalp))
    (if (db-query (prefix-db) nil !vann:preferredNamespacePrefix (wilbur:literal string))
      (cdr (assoc string (merge-resolved-prefixes) :test 'equalp))
      (let ((query-uri (format nil "http://prefix.cc/~A.file.vann" string)))
        (db-load (prefix-db) query-uri)
        (commit-prefix-db)
        (cdr (assoc string (merge-resolved-prefixes) :test 'equalp))))))

(defun all-resolved-prefixes ()
  (unless (assoc "vann" (all-prefixes) :test 'equalp)
    (add-namespace "vann" "http://purl.org/vocab/vann/"))
  (let (result)
    (dolist (triple (db-query (prefix-db) nil !rdf:type !owl:Ontology))
      (let ((pfx (w:db-get-values  (prefix-db) 
                   (triple-subject triple)
                   !vann:preferredNamespacePrefix))              
             (uri (w:db-get-values  (prefix-db) 
                    (triple-subject triple)
                    !vann:preferredNamespaceUri)))
        (push (cons (literal-string (car pfx)) (literal-string (car uri))) result)))
    (values result (length result))))

(defun merge-resolved-prefixes ()
  (let ((p (all-prefixes)))
    (dolist (rp (all-resolved-prefixes))
      (unless (assoc (car rp) p :test 'equalp)
        (add-namespace (car rp) (cdr rp)))))
  (all-prefixes))
