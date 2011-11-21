;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)


(def (method) pp ((string string) &optional (stream *standard-output*))
  "conveniently wraps pp-string in some generally sensible defaults"
  (pp-string stream string *PRINT-RIGHT-MARGIN*))


(def (function e) pp-string (stream strng width &optional indent (first-indent indent))
  "write string to stream, split into width-size lengths, breaking at
  returns and spaces in the string, if possible, indenting every line
  indent spaces (default = 0), except the first line which is indented
  first-indent spaces (default = indent).  Note: to generate a string
  simply use with-output-to-string or, the more general and extensible
  functionality provided by another function in this library: WRAP-STRING"
  (let ((*print-pretty* nil))
    (do* ((end (length strng))
           (indent-string
             (when (and indent (> indent 0))
               (make-string indent :initial-element #\space)))
           (first-indent-string
             (when (and first-indent (> first-indent 0))
               (make-string first-indent :initial-element #\space)))
           (start 0 (1+ next))
           (next (break-pos strng start end width)
             (break-pos strng start end width))
           (margin first-indent-string indent-string))
      ((null next))
      (when margin (write-string margin stream))
      (write-string strng stream :start start :end next)
      (terpri stream))))


(defun whitespace-p (ch)
 (member ch '(#\linefeed #\newline #\return #\space #\tab)))


(defun break-pos (strng start end width)
  "returns the position to break string at, guaranteed to be no more
  than width characters.  If there's a` return, its position is used,
  else the last space before the width cutoff, else width.  If the end
  comes before width, then end is returned."
  (unless (or (null start) (>= start end))
    (let ((limit (min (+ start width) end)))
      (or (position #\newline strng :start start :end limit)
        (and (= end limit) end)
        (position #\space strng :start start :end limit :from-end t)
        limit))))	


#|
;;; (non-whitespace-pos string &optional start) returns the position of
;;;   the first non-whitespace character in string, after start, if any.

;;; Not used now but was used before to set and update START in WRITE-WRAP
;;; to skip spaces. The current WRITE-WRAP keeps user spacing, except
;;; when replacing a space with a line break.

(defun non-whitespace-pos (strng &optional (start 0))
  (position-if-not #'whitespace-p strng :start start))

|#


#|
Copyright (c) 2003 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#




