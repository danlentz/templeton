(in-package :wilbur)
; (use-package "NET.HEXAPODIA.HASHTABLES")

(eval-when (:compile-toplevel :load-toplevel :execute)
    (defgeneric wilbur-hash (obj))
    (defgeneric wilbur-eql (a b))

    (defgeneric literal-hash (lit))

    (export 'wilbur-hash)
    (export 'wilbur-eql)
    (export 'wilbur::literal=)

    ;; Substitute for two missing combinations in Wilbur 2.
    ;; Note that we use literal-string rather than literal-value:
    ;; we don't want to try to do string= on a number!
    (defmethod wilbur::literal= ((literal string) (other-literal wilbur::literal))
      (string= (literal-string other-literal) literal))

    (defmethod wilbur::literal= ((literal wilbur::literal) (other-literal string))
      (string= (literal-string literal) other-literal)))
  
;; So, how do we hash a literal such that (test a b) => (= (hash a) (hash b))?
;; LITERAL= is the following:
;; Two strings: STRING=
;; Two literals: datatypes compared with EQL, literal values compared with
;; STRING=, and languages compared with STRING= (or check both are null).
;; One literal and one string: STRING= on literal values.

;; NB. hash equality does not imply that the test is true!
(defmethod literal-hash ((lit wilbur::literal))
  (sxhash (wilbur::literal-value lit)))
(defmethod literal-hash ((lit string))
  (sxhash lit))
  
;; Hash functions.
(defmethod wilbur-hash ((obj node))
  (sxhash obj))
(defmethod wilbur-hash ((obj wilbur::literal))
  (sxhash (wilbur::literal-value obj)))
(defmethod wilbur-hash ((obj string))
  (sxhash obj))
  
;; Equality functions.
(defmethod wilbur-eql (a b)
  (eq~ a b))
(defmethod wilbur-eql ((a string) (b string))
  (string= a b))
(defmethod wilbur-eql ((a string) (b wilbur::literal))
  (wilbur::literal= a b))
(defmethod wilbur-eql ((a wilbur::literal) (b string))
  (wilbur::literal= a b))
(defmethod wilbur-eql ((a wilbur::literal) (b wilbur::literal))
  (wilbur::literal= a b))

;; Buggeration. It seems that when a complete triple is provided to
;; Wilbur's stock DB-QUERY, and the object is a literal, the query
;; returns no results. This is doubtless because it's doing a literal
;; comparison with EQ, which is the default. Simply switch in WILBUR-EQL...
(defmethod db-query ((db indexed-db) subject predicate object)
  (flet ((filter (tr k s)
                 (if k (remove k tr :test-not #'wilbur-eql :key s) tr)))
    (declare (dynamic-extent #'filter))
    (cond (subject
            (filter (filter (triple-index-get (db-by-subject db) subject)
                            predicate #'triple-predicate)
                    object #'triple-object))
          (object
            (filter (triple-index-get (db-by-object db) object)
                    predicate #'triple-predicate))
          (predicate
            (triple-index-get (db-by-predicate db) predicate))
          (t
            (db-triples db)))))

(defmethod db-query ((db db) subject predicate object)
  (remove-if-not #'(lambda (triple)
                     (and (eq~ (triple-subject triple) subject)
                          (eq~ (triple-predicate triple) predicate)
                          (wilbur-eql (triple-object triple) object)))
                 (db-triples db)))

;; Register the nickname.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *set-hash* nil)
  (unless *set-hash* 

    (net.hexapodia.hashtables:register-hash-function 
      'wilbur-hash #'wilbur-hash #'wilbur-eql)

    (setq *set-hash* t)))

;; Now redefine the fake class TRIPLE-INDEX.
;; Just like TRIPLE-INDEX, but it works with nodes and literals.
;; We rely on GENHASH's generic functions working on the normal
;; hashes, too.

;; One new addition: this one makes a hash suitable for literals.
(defun wilbur::make-wilbur-triple-index ()
  (net.hexapodia.hashtables:make-generic-hashtable 
    :test 'wilbur-hash))
(defun wilbur::triple-index-get (index key)
  (net.hexapodia.hashtables:hashref key index))
(defun wilbur::triple-index-add (index key triple)
  (push triple (net.hexapodia.hashtables:hashref key index)))
(defun wilbur::triple-index-rem (index key triple)
  (unless (stringp key)
    (setf (net.hexapodia.hashtables:hashref key index)
          (remove triple 
                  (net.hexapodia.hashtables:hashref key index)))))
(defun wilbur::triple-index-clear (index)
  (net.hexapodia.hashtables:hashclr index))
  
;; REDEFINE DB!
(defclass indexed-db (db)
  ((by-subject
    :initform (make-triple-index)
    :reader db-by-subject)
   (by-predicate
    :initform (make-triple-index)
    :accessor db-by-predicate)
   ;; Object triple index now takes our new hash.
   (by-object
    :initform (make-wilbur-triple-index)
    :accessor db-by-object)
   (by-source
    :initform (make-triple-index)
    :reader db-by-source))
  (:default-initargs
    :emptyp t
    :rdf-schema-pathname "wilbur:schemata;rdf-schema.rdf"))

;; We want an indexed literal database to start with.
(let ((new-db (make-instance 'indexed-db :emptyp t)))
  (when (and (boundp 'wilbur:*db*) wilbur:*db*)
    (db-merge new-db wilbur:*db*))
  (setf wilbur:*db* new-db))
