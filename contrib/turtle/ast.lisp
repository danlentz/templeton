;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; abstract syntax tree

(in-package :gs.ebu.wilbur.turtle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Growing an Abstract Syntax Tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun pass-through (x)
  x)

(defun build-blank (&optional contents)
  (cons :blank contents))

(defun build-nodeid (nodeid)
  (list :nodeid nodeid))

(defun build-qname (r)
  (list :qname r))

(defun build-uriref (r)
  (list :uriref r))

(defun build-object-list (object &optional objects)
  (list* :objects object (rest objects)))

(defun build-collection (objects)
  (list* :collection objects))

(defun build-turtle (statements)
  (cons :turtle statements))

(defun build-triples (subject predicate-objects-lists)
  (cons :triples (cons subject predicate-objects-lists)))

(defun build-predicate-object-list (predicate object-list &optional predicate-object-lists)
  (cons (list :predicate-object-list predicate object-list)
        predicate-object-lists))

(defun build-boolean (bool)
  (list :boolean bool))

(defun build-integer (integer)
  (list :integer (read-from-string integer)))

(defun build-decimal (decimal)
  (list :decimal (read-from-string decimal)))

(defun build-double (double)
  (list :double (read-from-string double)))

(defun build-string (string &optional language)
  (list* :string (subseq string 1 (- (length string) 1))
         (if language
             (list (subseq language 1)))))

(defun build-long-string (string &optional language)
  (list* :string (subseq string 3 (- (length string) 3))
         (if language
             (list (subseq language 1)))))

(defun build-datatype-string (string datatype)
  (list :datatype-string string datatype))


