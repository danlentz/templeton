
;;;;; turtle-lexer
;;;;;
;;;;; Maintainer:  <danlentz@gmail.com>
;;;;;
;;;;; Portions Copyright © 2009 The Open University

(in-package :gs.ebu.wilbur.turtle)


(defun parse-turtle-string (turtle-string)
  (yacc:parse-with-lexer (turtle-lexer turtle-string) turtle-parser))


(defclass turtle-parser ()
  ((db :initarg :db
       :initform nil
       :accessor wilbur::parser-db)))


(defmethod wilbur::parse ((parser turtle-parser) stream locator)
  (setf (wilbur::parser-db parser)
        (turtle-ast-to-rdf-db
         (parse-turtle-string (read-stream-to-string stream)))))

(defun parse-turtle-stream (parser stream locator)
  (wilbur::parse parser stream locator))
