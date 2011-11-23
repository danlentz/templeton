;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)

(defvar *INSTANCE-PREFIX*             "_")
(defvar *INSTANCE-URI*                (format nil "~A~A" (short-site-name) "resource/"))
(defvar *DEFAULT-VANN-STORAGE-GRAPH*  (format nil "~A~A" *INSTANCE-URI* "vann#"))

(defvar +null-namespace+              (unicly:make-null-uuid))
(defvar +node-namespace+              unicly:*UUID-NAMESPACE-URL*)
(defvar +dns-namespace+               unicly:*UUID-NAMESPACE-DNS*)
(defvar +oid-namespace+               unicly:*UUID-NAMESPACE-OID*)
(defvar +statement-namespace+         (unicly:make-v5-uuid +node-namespace+ -rdf-statement-uri-))
(defvar +literal-namespace+           (unicly:make-v5-uuid +node-namespace+ -rdfs-literal-uri-))
(defvar +local-namespace+             (unicly:make-v5-uuid +dns-namespace+  *INSTANCE-URI*))

