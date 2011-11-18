;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;; This code has been placed in the public domain by the author.
;;; It is distributed without warranty of any kind.
;;;
;;; Description: Simple Histogram facility.  Just prints out the result
;;;   using Format.  No fancy graphics.
;;;
;;; Author: Scott E. Fahlman
;;;
;;; Current maintainer:	Scott E. Fahlman
;;;
;;; Address: Carnegie-Mellon University
;;;          Computer Science Department
;;;	     Pittsburgh, PA 15213
;;;
;;; Net address: Scott.Fahlman@cmu-cs-a
;;;
;;; Copyright status: Public domain.
;;;
;;; Compatibility: Should run in any legal Common Lisp implementation.
;;;
;;; Dependencies: Depends only on standard Common Lisp facilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :templeton)

(def (special-variable) *hist-lower-limit*)
(def (special-variable) *hist-upper-limit*)
(def (special-variable) *hist-bucket-size*)
(def (special-variable) *hist-nbuckets*)
(def (special-variable) *hist-array*)

(def (constant) +histogram-bar-chars-limit+ 72
  "The maximum number of #'s that are to be printed.")

(def (macro e) with-histogram-output ((low high &optional (bucket-size 1)) &body body)
  "Format is (HIST (low high [bucket-size]) . body).  Creates a
histogram with buckets of the specified size (defaults to 1), spanning
the range from Low (inclusive) to High (exclusive), with two
additional buckets to catch values below and above this range.  The
body is executed as a progn, and every call to Histogram-Record-Datum
within the body provides a value for the histogram to count.  When
Body exits, the histogram is printed out and Hist returns Nil."
     `(let* ((*hist-lower-limit* ,low)
             (*hist-upper-limit* ,high)
             (*hist-bucket-size* ,bucket-size)
             (stream *standard-output*)
              (*hist-nbuckets*
                (+ 2 (ceiling (- *hist-upper-limit* *hist-lower-limit*) *hist-bucket-size*)))
             (*hist-array* (make-array *hist-nbuckets* :initial-element 0)))
        (progn ,@body)
        (let ((biggest 0) (scale 1))
          (dotimes (b (- *hist-nbuckets* 2))
            (when (> (svref *hist-array* b) biggest)
              (setq biggest (svref *hist-array* b))))
          (when (> biggest +histogram-bar-chars-limit+)
            (setq scale (ceiling biggest +histogram-bar-chars-limit+))
            (format stream "~&Each \"#\" equals ~S units.  The \".\" indicates a fraction." scale))
          (format stream "~&< ~S: ~12,8T~S~%"
            *hist-lower-limit* (svref *hist-array* (1- *hist-nbuckets*)))
          (do ((b 0 (1+ b))
               (bval *hist-lower-limit* (+ bval *hist-bucket-size*))
               (bcount 0))
              ((= b (- *hist-nbuckets* 2)))
            (setq bcount (svref *hist-array* b))
            (multiple-value-bind (q r) (truncate bcount scale)
              (format stream "~S: ~12,8T~S~20,8T~V,1,0,'#@A~%" bval bcount
                (1+ q) (if (zerop r) #\  #\.))))
          (format stream "> ~S: ~12,8T~S~%" *hist-upper-limit*
            (svref *hist-array* (- *hist-nbuckets* 2))))
        (values)))


(def (function e) histogram-record-datum (value)
  "This function should only be called within the body of a HIST form.
  Increments the proper histogram counter to record this value."
  (cond ((< value *hist-lower-limit*)
	 (incf (svref *hist-array* (1- *hist-nbuckets*))))
	((>= value *hist-upper-limit*)
	 (incf (svref *hist-array* (- *hist-nbuckets* 2))))
	(t (incf (svref *hist-array* (floor (- value *hist-lower-limit*)
					    *hist-bucket-size*))))))



(def (function) test-histogram ()
     (with-histogram-output (0 100 10) 
       (dotimes (i 50) 
         (histogram-record-datum (random 100 (make-random-state t)))))
     (values))

#|

(test-histogram)

< 0:        0
0:          4       #### 
10:         4       #### 
20:         4       #### 
30:         5       ##### 
40:         6       ###### 
50:         6       ###### 
60:         8       ######## 
70:         5       ##### 
80:         3       ### 
90:         5       ##### 
> 100:      0

|#
