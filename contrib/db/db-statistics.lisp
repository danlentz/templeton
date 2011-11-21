;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;; --------------------------------------------------------------------------------------


(in-package :templeton)

(export '(db-access-counter-mixin db-reset-counters db-access-stats db-index-stats))

;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defclass db-access-counter-mixin ()
  ((low-level-counters
    :initform (make-hash-table :test #'equal)
    :reader db-low-level-counters)
   (low-level-access-count
    :initform 0
    :accessor db-low-level-access-count)
   (high-level-counters
    :initform (make-hash-table :test #'equal)
    :reader db-high-level-counters)))

(defgeneric db-reset-counters (db))

(defmethod db-reset-counters ((db db-access-counter-mixin))
  (setf (db-low-level-access-count db) 0)
  (clrhash (db-low-level-counters db))
  (clrhash (db-high-level-counters db)))

(defmethod db-query :after ((db db-access-counter-mixin) subject predicate object)
  (incf (db-low-level-access-count db))
  (unless (and subject predicate object)
    (incf (gethash (list subject predicate object)
		   (db-low-level-counters db)
		   0))))

(defmethod db-get-values :after ((db db-access-counter-mixin) node path)
  (let* ((cache (db-high-level-counters db))
	 (sub-cache (or (gethash path cache)
			(setf (gethash path cache) (make-hash-table :test #'eq)))))
    (incf (gethash node sub-cache 0))))

(defclass simple-test-db (db-access-counter-mixin
			  interned-literal-indexed-db)
  ())

(defgeneric db-access-stats (db))

(defmethod db-access-stats ((db db-access-counter-mixin))
  (let ((s 0)
	(sp 0)
	(so 0)
	(p 0)
	(po 0)
	(o 0)
	(any 0))
    (maphash #'(lambda (pattern count)
		 (destructuring-bind (subject predicate object) pattern
		   (cond (subject
			  (cond (predicate
				 (assert (null object) nil
					 "Should not come here: ~S" pattern)
				 (incf sp count))
				(object
				 (incf so count))
				(t
				 (incf s count))))
			 (predicate
			  (if object
			    (incf po count)
			    (incf p count)))
			 (object
			  (incf o count))
			 (t
			  (incf any count)))))
	     (db-low-level-counters db))
    `(:any ,any 
      :s ,s :sp ,sp :spo ,(- (db-low-level-access-count db) s sp so p po o any) :so ,so
      :p ,p :po ,po :o ,o)))


;;; --------------------------------------------------------------------------------------
;;;
;;;   
;;;

(defgeneric db-index-stats (indexed-db))

(defmethod db-index-stats ((db indexed-db))
  (let ((n-hashtables 0)
	(size-hashtables 0)
	(total-size-hashtables 0))
    (labels ((add-index (i)
	       (incf total-size-hashtables (hash-table-size i))
	       (incf size-hashtables (hash-table-count i))
	       (incf n-hashtables)
	       #+:junk
	       (maphash #'(lambda (key triples)
			    (declare (ignore key))
			    nil)
			i))
	     (add-sub-indices (i)
	       (maphash #'(lambda (key sub-index)
			    (declare (ignore key))
			    (add-index sub-index))
			i)))
      (add-index (w::db-index-s db))
      (add-index (w::db-index-p db))
      (add-index (w::db-index-o db))
      (add-sub-indices (w::db-index-sp db))
      (add-sub-indices (w::db-index-po db))
      (add-index (w::db-by-source db))
      (list :n n-hashtables :count size-hashtables :size total-size-hashtables))))
