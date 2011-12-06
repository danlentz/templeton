;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :cl-user)

(macrolet ((ensure-package (name &rest nicknames)
             `(or (find-package ,name) (make-package ,name :nicknames ,@nicknames))))
  (let* ((lexical-space (short-site-name))
          (node-space-identifier (unicly:make-v5-uuid unicly:*uuid-namespace-dns* lexical-space))
          (node-space (ensure-package lexical-space
                        (list (princ-to-string node-space-identifier) "_" "urn:uuid"))))
    ;; (define-symbol-macro :_ (find-package "_"))
    (define-symbol-macro _ (find-package "_"))
    (shadowing-import 'screamer::defun node-space)
    (use-package :screamer node-space)))





(DEFPACKAGE #:TEMPLETON  (:NICKNAMES #:TTON)
  
  (:USE #:ELEPHANT #:HU.DWIM.STEFIL #:COMMON-LISP #:CLOSER-MOP #:WILBUR #:HU.DWIM.DEF
    #:NAMED-READTABLES)

  (:SHADOWING-IMPORT-FROM #:WILBUR #:GET-VALUE)
  (:SHADOWING-IMPORT-FROM :CLOSER-MOP :DEFMETHOD :DEFGENERIC :STANDARD-GENERIC-FUNCTION)    

  (:IMPORT-FROM #:WILBUR #:DB-ADD-NAMESPACE #:DB-COUNT-TRIPLES #:DB-FIND-TRIPLE
    #:DB-LITERAL-CLASS #:DB-MAKE-LITERAL #:DB-NEW-CONTAINER-MEMBERSHIP-PROPERTY
    #:DB-NODE-RESOLVED #:DB-PATH-FSAS)

  (:EXPORT
    
    #:PRINTV
    #:ALL-NAMED-GRAPHS
    #:ALL-PREFIXES
    #:RESOLVE-PREFIX
    #:MERGE-RESOLVED-PREFIXES

    #:PREFIX-DB
    #:EXTENDED-DB
    #:DB-ACCESS-STATS
    #:DB-ADD-NAMESPACE
    #:DB-ADD-TRIPLE
    #:DB-CLEAR
    #:DB-COUNT-TRIPLES
    #:DB-DEL-SOURCE
    #:DB-DEL-TRIPLE
    #:DB-FIND-CBD
    #:DB-FIND-TRIPLE
    #:DB-INDEX-STATS
    #:DB-LITERAL-CLASS
    #:DB-MAKE-LITERAL
    #:DB-MAKE-TRIPLE
    #:DB-MERGE
    #:DB-NEW-CONTAINER-MEMBERSHIP-PROPERTY
    #:DB-NODE-RESOLVED
    #:DB-OF
    #:DB-PATH-FSAS
    #:DB-QUERY
    #:DB-QUERY-BY-SOURCE
    #:DB-SOURCE-DESCS
    #:DB-SOURCES
    #:DB-TRIPLES

    #:DICTIONARY-CLEAR
    #:DICTIONARY-SIZE

    #:MAKE-FRAME
    #:FRAME-CBD
    #:FRAME-DROP
    #:FRAME-PUT
    #:FRAME-RELATED-P
    #:FRAME-SLOTS
    #:FRAME-VALUE
    #:FRAME-VALUES        
    #:WITH
    #:WITH-GRAPH
    #:WITH-MERGED-GRAPHS
    
    #:GRAPH-CONTAINER
    #:LOAD-ONTOLOGY
    #:NAMED-GRAPH
    #:NAMED-GRAPH-DB
    #:NG
    #:NGDB
    #:ANG
    
    #:PHASH
    #:PINDEX
    #:MAKE-PHASH
    #:MAKE-PINDEX
    #:PHASH-PUT
    #:PHASH-DROP
    #:PHASH-GET
    #:PHASH-GET*
    #:PHASH-CLEAR
    #:PINDEX-CLEAR   
    #:PHASH-MAP
    #:PHASH-SIZE
    #:BTREE-INFO
    
    #:REGISTER-OBJECT
    #:UNREGISTER-OBJECT
    #:ROLLBACK
    #:COMMIT
    #:SNAPSHOT-ROOT
    #:SNAPSHOT-SET

    #:STORABLE-GRAPH
    #:DROP-ALL-NAMED-GRAPHS
    #:DROP-GRAPH
    #:STORABLE-NODE
    #:STORABLE-NODE-DICTIONARY

    :make-string-builder
    :with-string-builder
    :with-string-builder-output
    :make-reducer
    :with-reducer
    :make-collector
    :make-appender
    :make-pusher
    :with-appender
    :with-collector
    :with-collectors
    :with-mapping-collector
    :with-mapping-appender

    :null-mapper
    :compose-mappers
    :concatente-mappers
    :curry-mapper
    :make-mapper
    :encode-mapper
    :map-lines))
