;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :templeton)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic TTY display protocol definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def (generic ed) pp (thing &optional stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specialied TTY display implementations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def method pp ((table hash-table) &optional (stream *standard-output*))
  "Print a hash table to a stream, defaulting to *standard-output*"
  (format stream "~%~A~%" table)
  (maphash #'(lambda (key value)
               (format stream "~&;; ~S => ~S~%" key value))
    table))


(def method pp ((array array) &optional (stream *standard-output*))
  "Print `array' to `stream' (which defaults to *standard-output*)."
  (let ((*print-array* t))
    (print array stream)))

(def (function e) pprint-alist (alist &optional stream)
  (terpri stream)
  (princ "(" stream)
  (loop for sublist in alist do
        (format (or stream t) "~& (~10@<~S~> . ~6@<~S~>) ~%"
                (car sublist)
                (cdr sublist)))
  (princ ")" stream)
  (terpri stream)
  (values))


(def (function e) print-n-chars (char n stream)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-char char stream)))


(def (function e) print-n-strings (str n stream)
  (declare (fixnum n) (optimize (speed 3) (safety 0) (space 0)))
  (dotimes (i n)
    (declare (fixnum i))
    (write-string str stream)))


(defun indent-2n-spaces (n &optional (stream *standard-output*))
  "Indent n*2 spaces to output stream"
  (print-n-chars #\space (+ n n) stream))

(defun print-list (list &optional (output *standard-output*))
  "Print a list to a stream"
  (format output "~{~A~%~}" list))

(defun print-rows (rows &optional (stream *standard-output*))
  "Print a list of list rows to a stream"
  (dolist (r rows) (format stream "~{~A~^ ~}~%" r)))




#|
(def method pp ((ht hash-table) &optional (stream t) &aux previous out)
;;  (let ((previous nil)
  (setf       out
           (with-output-to-string (buf)
             (let ((*standard-output* buf))
               (pprint-logical-block (nil nil :prefix "(" :suffix ")")
                 (maphash #'(lambda (key value)
                              (pprint-pop)
                              (if previous (write-char #\Space)
                                (setf previous t))
                              (write key)
                              (write-char #\Space)
                              (write value)
                              (pprint-newline :fill))
                   ht)))))
    (if stream (progn (princ out stream) (values))
      out))

;)

(pp (ngdb !rdf:))
"(\"ns\" !\"ns\" \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" !rdf:)"
      
  
|#
