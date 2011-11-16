;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;

(in-package :cl-user)

(defpackage "_" (:use))


(DEFPACKAGE #:TEMPLETON
  (:NICKNAMES :TTON)
  (:SHADOWING-IMPORT-FROM #:WILBUR #:GET-VALUE)
  (:USE #:ELEPHANT #:HU.DWIM.STEFIL #:COMMON-LISP #:WILBUR #:HU.DWIM.DEF)
  (:IMPORT-FROM :WILBUR.TURTLE #:TURTLE-DB #:TURTLE-PARSER #:BNODES-OF #:INTERN-BNODE 
    #:PARSE-TURTLE-STRING #:PARSE-TURTLE-STREAM )
  (:EXPORT #:*GRAPHS*
;;    #:ALL
    #:ALL-NAMED-GRAPHS
    #:ALL-PREFIXES
;;    #:CLEAR-CACHE
;;    #:CLEAR-TOUCHED
;;    #:COLLECT-UNTOUCHED
    #:COMMIT
;;    #:COMMIT-GRAPH
    #:DB-ACCESS-STATS
    #:DB-INDEX-STATS
    #:DB-OF
    #:DICTIONARY-CLEAR
    #:DICTIONARY-SIZE
    #:DROP-GRAPH
    #:DROP-ALL-NAMED-GRAPHS
;;    #:DROP-CACHED-OBJECT
    #:EXTENDED-DB
;;    #:FIND-GRAPH
;;    #:GET-CACHE-ENTRIES
    #:GET-PHASH
    #:GET-PHASH*
    #:GRAPH-CONTAINER
    #:IMPORT-GRAPH
    #:LOAD-SCHEMA
;;    #:LOOKUP-CACHED-OBJECT
;;    #:MAKE-GRAPH
    #:MAKE-PHASH
    #:MAKE-PINDEX
    #:MAP-PHASH
;;    #:MAP-SET
    #:MERGE-RESOLVED-PREFIXES
    #:NAMED-GRAPH
    #:NAMED-GRAPH-DB
    #:NDB
    #:NG
    #:ANG
    #:PHASH
    #:PHASH-HELP
    #:PHASH-MAP
    #:PHASH-SIZE
    #:PINDEX
    #:PINDEX-MAP
    #:PREFIX-DB
    #:PRINTV
;;    #:PUT-GRAPH
    #:PUT-PHASH
;;    #:REGISTER-OBJECT
    #:REM-PHASH
    #:RESOLVE-PREFIX
    #:ROLLBACK
;;    #:ROLLBACK-GRAPH
;;    #:RUN-SNAPSHOT-TESTS
    #:SNAPSHOT-COMMIT
    #:SNAPSHOT-RESTORE
    #:SNAPSHOT-ROOT
    #:SNAPSHOT-SET
    #:STORABLE-GRAPH
    #:STORABLE-NODE
    #:STORABLE-NODE-DICTIONARY
;;    #:TOUCH
;;    #:UNREGISTER-OBJECT
    #:TURTLE-DB
    #:TURTLE-PARSER
    #:BNODES-OF
    #:INTERN-BNODE 
    #:PARSE-TURTLE-STRING
    #:PARSE-TURTLE-STREAM
    ))
