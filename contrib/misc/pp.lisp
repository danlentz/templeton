;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :templeton)

(def (generic ed) pp (o &optional stream))

(def method pp ((ht hash-table) &optional stream)
  (let (previous (out (with-output-to-string (buf)
      (let ((*standard-output* buf))
        (pprint-logical-block (nil nil :prefix "(" :suffix ")")
          (maphash (lambda (key value)
                     (pprint-pop)
                     (if previous
                       (write-char #\Space)
                       (setf previous t))
                     (write key)
                     (write-char #\Space)
                     (write value)
                     (pprint-newline :fill))
            hash-table))))))
    (if stream (progn (print out stream) (values)) out)))
      
  
