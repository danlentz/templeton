;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :cl-user)

(defun ensure-nodespace ()
  (macrolet ((ensure-package (name &rest nicknames)
               `(or (find-package ,name) (make-package ,name :nicknames ,@nicknames))))
    (let* ((lexical-space (short-site-name))
            (node-space-identifier (unicly:make-v5-uuid unicly:*uuid-namespace-url* lexical-space))
            (node-space (ensure-package lexical-space
                          (list (princ-to-string node-space-identifier) "_" "urn:uuid"))))
      ;; (define-symbol-macro :_ (find-package "_"))
      (defun _ () (find-package "_"))
      (defparameter _ (_))
      (export '_)
      (let ((ns-symbol  (intern (princ-to-string node-space-identifier) _)))
        (set ns-symbol _)
        (export ns-symbol _)
        (describe ns-symbol))      
      (import 'cl-user::_ _)
      (export 'cl-user::_ _)
      (shadowing-import 'screamer::defun node-space)
      (export 'screamer::defun node-space)
      (use-package :screamer node-space)
      (do-external-symbols (s (find-package :screamer))
        (export s node-space))
      _:_)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (describe (ensure-nodespace)))

#|

;;
;; ===============================================================================================
;;   (DESCRIBE (_:_)) =>
;; #<PACKAGE "http://ebu.gs/">
;;   [package]
;;
;; Nicknames: 7c7de744-9eea-5a23-ab56-ae806c69bb0a, _, urn:uuid
;; Use-list: SCREAMER
;; Shadows: DEFUN
;; Exports: *DYNAMIC-EXTENT?*, *ISCREAM?*, *MAXIMUM-DISCRETIZATION-RANGE*, *MINIMUM-SHRINK-RATIO*,
;;          *SCREAMER-VERSION*, *STRATEGY*, *V, +V, -V, /=V, /V, <=V, <V, =V, >=V, >V, A-BOOLEAN,
;;          A-BOOLEANV, A-MEMBER-OF, A-MEMBER-OFV, A-NUMBERV, A-REAL-ABOVEV, A-REAL-BELOWV,
;;          A-REAL-BETWEENV, A-REALV, ALL-VALUES, AN-INTEGER, AN-INTEGER-ABOVE, AN-INTEGER-ABOVEV,
;;          AN-INTEGER-BELOW, AN-INTEGER-BELOWV, AN-INTEGER-BETWEEN, AN-INTEGER-BETWEENV, 
;;          AN-INTEGERV, ANDV, APPLY-NONDETERMINISTIC, APPLY-SUBSTITUTION, APPLYV, ASSERT!, 
;;          BEST-VALUE, BOOLEAN, BOOLEANP, BOOLEANPV, BOUND?, COUNT-FAILURES, COUNT-TRUES, 
;;          COUNT-TRUESV, DECIDE, DEFINE-SCREAMER-PACKAGE, DEFUN, DIVIDE-AND-CONQUER-FORCE, 
;;          DOMAIN-SIZE, EITHER, EQUALV, FAIL, FOR-EFFECTS, FUNCALL-NONDETERMINISTIC, FUNCALLV, 
;;          GLOBAL, GROUND?, INTEGERPV, ITH-VALUE, KNOWN?, LINEAR-FORCE, LOCAL, LOCAL-OUTPUT, 
;;          MAKE-VARIABLE, MAXV, MEMBERV,  MINV, MULTIPLE-VALUE-CALL-NONDETERMINISTIC, 
;;          NECESSARILY?, NONDETERMINISTIC-FUNCTION?, NOTV, NUMBERPV, ONE-VALUE, ORV, POSSIBLY?, 
;;          PRINT-VALUES, PURGE, RANGE-SIZE, REALPV, REORDER, SOLUTION, STATIC-ORDERING, TEMPLATE, 
;;          TRAIL, UNWEDGE-SCREAMER, UNWIND-TRAIL, VALUE-OF, WHEN-FAILING, _,
;;          7c7de744-9eea-5a23-ab56-ae806c69bb0a
;; 0 internal symbols.
;;  [returned 0 values]
;;
;; ===============================================================================================
;;
;;   _:|7c7de744-9eea-5a23-ab56-ae806c69bb0a|   => #<PACKAGE "http://ebu.gs/">
;;   (find-package :_)                          => #<PACKAGE "http://ebu.gs/">
;;   (_:_)                                      => #<PACKAGE "http://ebu.gs/">
;;    _:_                                       => #<PACKAGE "http://ebu.gs/">
;; 
;; ===============================================================================================
;;
;;   (_:defun _::TRUE () T)                      => |http://ebu.gs/|::TRUE
;;   (defparameter _::TRUE (_:make-variable))    => |http://ebu.gs/|::TRUE 
;;   (export '_::TRUE _:_)                       => T
;;   (_:assert! (_:equalv _:TRUE (_:TRUE)))      => NIL
;;
;;   _:TRUE                  => T
;;   (_:TRUE)                => T
;;
;; -----------------------------------------------------------------------------------------------
;;
;;   (describe '|http://ebu.gs/|:TRUE)
;;
;; |http://ebu.gs/|:TRUE
;;   [symbol]
;;
;; TRUE names a special variable:
;;   Value: T
;;
;; TRUE names a compiled function:
;;   Lambda-list: ()
;;   Derived type: (FUNCTION NIL (VALUES (MEMBER T) &OPTIONAL))
;;

|#



(DEFPACKAGE #:TEMPLETON  (:NICKNAMES #:TT)
  
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
