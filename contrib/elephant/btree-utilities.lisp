;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;; 
;;; Initial version 3/29/2006 Ian Eslick
;;; License: Lisp Limited General Public License (LLGPL)

(in-package :templeton)


(defparameter *base-types* '(persistent-object persistent-collection
    structure-object standard-object number string array hash-table))

(def (generic ed) btree-info (btree &key print-depth search-depth dump recurse))

(defun make-btree-summary-record ()
  (let ((record nil))
    (loop for type in *base-types* do
	 (push (cons (intern (string type) (find-package 'keyword)) 0)
	       record))
    record))

(defun update-stats-for-value (value record)
  (loop for type in *base-types* do
       (when (subtypep (type-of value) type)
	 (incf (cdr (assoc (intern (string type) (find-package 'keyword)) record))))))


(def method btree-info ((btree btree) &key (print-depth 100) (search-depth nil)
                         (dump nil) (recurse nil))
  (let ((count 0) (record (make-btree-summary-record)))
    (catch 'max-depth 
      (map-btree
        (lambda (key val)
          (incf count)
          (when (and search-depth (> count search-depth))
            (throw 'max-depth nil))
          (update-stats-for-value val record)
          (when (and dump (< count print-depth))
            (format t "key: ~A  value: ~A~%" key val))
          (when (and recurse (subtypep (type-of val) 'btree) (< count print-depth))
            (format t "Recursing into ~A:~A...~%" key val)
            (btree-info val :search-depth search-depth :print-depth print-depth 
              :dump dump :recurse recurse)
            (format t "...completing recursion into ~A:~A~%" key val)))
        btree))
    (format t "Summary:~%")
    (loop
      for i from 1
      for pair in record do
      (cond ((eq (car pair) 'array)
              (format t "~A (~A)~%"
                (symbol-name (car pair)) (- (cdr pair) (cdr (assoc 'string record)))))
        (t (format t "~22A (~3A)   " (symbol-name (car pair)) (cdr pair))
          (when (eq (mod i 3) 0) (terpri))
          )))))


