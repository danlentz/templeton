;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)



(def (special-variable) *bar-graph-width* 80)

(defun total-count (alist)
  (reduce #'+ alist :key #'second))

(defun max-value (alist)
  (reduce #'max alist :key #'second))

(defun max-length (alist)
  (reduce #'max alist :key
          (lambda (x) (length (princ-to-string  (car x))))))

(defun row-length (value total)
  (truncate (* *bar-graph-width* value) total))

(defun percent (value total)
  (/ (* 100 value) total))

(defun make-bar (value total pad)
  (make-string (max 0 (- (row-length value total) pad))
               :initial-element #\-))

(def (function e) pprint--bar-graph (alist &key (stream t)  title (key #'identity) end)
  "takes as input an alist containing (name . value) pairs"
  (when alist
   (let* ((total (total-count alist))
          (alist (subseq alist 0 end))
          (max-value (max-value alist))
          (max (funcall key max-value))
          (max-length (max-length alist))
          (pad (+ max-length 3)))
     (when title
       (format stream "~vt~a~2%" (floor (- *bar-graph-width* (length title)) 2) title))
     (loop for (name value) in alist
           do
           (format stream "~v@a ~ao~vt~a~vt~5,2f%~%"
                   max-length
                   name
                   (make-bar (funcall key value) max pad)
                   *bar-graph-width*
                   value
                   (+ 2 *bar-graph-width* (length (write-to-string max-value)))
                   (percent value total)))
     (format stream "----~%Total: ~a~%" total)
     (values))))
