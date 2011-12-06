;;; :FILE-CREATED <Timestamp: #{2011-05-23T20:18:33-04:00Z}#{11211} - by MON>
;;; :FILE dbc-class-uuid-namespace-for-control-id.lisp
;;; ==============================
;;(delete-package :id)


(defpackage :templeton.namespace
  (:nicknames :ns)
  (:use :unicly :common-lisp)
  (:export
    :contextual-object-uuid
    :contextual-identity
    :contextual-identity-parent-uuid
    :contextual-identity-uuid
    :contextual-identity-uuid-byte-array
    :contextual-identity-uuid-bit-vector
    :contextual-identity-uuid-integer
    :contextual-identity-uuid-string-36
    :contextual-identity-uuid-version
    :contextual-object-uuid-description
    :make-contextual-object-uuid))

(in-package :templeton.namespace)
;;; ==============================
;;
#|
(define-symbol-macro bubba#
  (defparameter _::bubba#
    (make-contextual-object-uuid :base-namespace unicly:*uuid-namespace-url*
      :control-id "bubba")))
*TT--CONTEXTUAL-OBJ-UUID*
|#
;; => *TT--CONTEXTUAL-OBJ-UUID*
;;
;; *TT--CONTEXTUAL-OBJ-UUID*
;; |=> #<CONTEXTUAL-OBJECT-UUID 
;; |     CONTEXTUAL-ID:      "bubba"
;; |     CONTEXTUAL-UUID:    eea1105e-3681-5117-99b6-7b2b5fe1f3c7>
;;
;; (unicly:uuid-eql (make-v5-uuid (contextual-identity-parent-uuid  *tt--contextual-obj-uuid*)
;;                                (contextual-identity  *tt--contextual-obj-uuid*))
;;                  (contextual-identity-uuid *tt--contextual-obj-uuid*))
;; => T
;;
;; (setf *tt--contextual-obj-uuid*
;;       (make-contextual-object-uuid :base-namespace unicly:*uuid-namespace-dns* 
;;                                :control-id 'bubba))
;; |=> #<CONTEXTUAL-OBJECT-UUID 
;; |     CONTEXTUAL-ID:      BUBBA
;; |     CONTEXTUAL-UUID:    60ada823-d6de-5729-9e7e-8e44a57e400d >
;;
;; (unicly:uuid-eql (make-v5-uuid (contextual-identity-parent-uuid  *tt--contextual-obj-uuid*)
;;                                 (string (contextual-identity  *tt--contextual-obj-uuid*)))
;;                   (contextual-identity-uuid *tt--contextual-obj-uuid*))
;; => T
;;
;; (setf (contextual-identity *tt--contextual-obj-uuid*)
;;       (list (make-v4-uuid) "new-bubba"))
;; => "new-bubba"
;;
;; *tt--contextual-obj-uuid*
;;  |=> #<CONTEXTUAL-OBJECT-UUID 
;;  |    :CONTEXTUAL-IDENTITY             "new-bubba"
;;  |    :CONTEXTUAL-IDENTITY-UUID        0ef86c6f-b263-5987-a90c-3fcca581bc38 >
;;
;; (contextual-identity-uuid  *tt--contextual-obj-uuid*)
;; ; => 0ef86c6f-b263-5987-a90c-3fcca581bc38
;;
;; (contextual-identity  *tt--contextual-obj-uuid*)
;;  ;=> "new-bubba"
;;
;; (contextual-identity-parent-uuid  *tt--contextual-obj-uuid*)
;;  ;=> addedf68-81a4-47cd-a7b1-b96779f8b676
;; 
;; (type-of (contextual-identity-parent-uuid  *tt--contextual-obj-uuid*))
;; => UNIQUE-UNIVERSAL-IDENTIFIER
;;
;; (unicly:uuid-version-uuid (contextual-identity-parent-uuid  *tt--contextual-obj-uuid*))
;; ;=> 4
;;
;; (contextual-identity-uuid-version *tt--contextual-obj-uuid*)
;; ;=> 5
;;
;; (contextual-identity-uuid-byte-array  *tt--contextual-obj-uuid*)
;; ;=> #(14 248 108 111 178 99 89 135 169 12 63 204 165 129 188 56)
;;
;; (contextual-identity-uuid-bit-vector  *tt--contextual-obj-uuid*)
;; ;=> #*000011101111100001101100011011111011001001100011010 ...
;;
;; (contextual-identity-uuid-integer     *tt--contextual-obj-uuid*)
;; ;=> 19899080911677131133725998230922181688
;;
;; (contextual-identity-uuid-string-36   *tt--contextual-obj-uuid*)
;; ;=> "0ef86c6f-b263-5987-a90c-3fcca581bc38"
;;
;; (update-contextual-object-uuid *tt--contextual-obj-uuid*
;;                            :base-namespace (make-v5-uuid *uuid-namespace-dns* (string '*tt--contextual-obj-uuid*))
;;                            :control-id '*tt--contextual-obj-uuid*)
;; |=> #<CONTEXTUAL-OBJECT-UUID 
;; |    :CONTEXTUAL-IDENTITY             *TT--CONTEXTUAL-OBJ-UUID*
;; |    :CONTEXTUAL-IDENTITY-UUID        db197774-6955-55b1-ac30-143864977f41 >
;;
;;
;; (contextual-identity *TT--CONTEXTUAL-OBJ-UUID*)
;; ;=> *TT--CONTEXTUAL-OBJ-UUID*
;;
;; (eq (contextual-identity *TT--CONTEXTUAL-OBJ-UUID*) '*TT--CONTEXTUAL-OBJ-UUID*)
;; ;=> T
;;
;;; ==============================


;; *package*

(defclass base-uuid (unicly:unique-universal-identifier)())

;;;; The class CONTEXTUAL-OBJECT-UUID
;;
;; contextual-object-uuid               <CLASS>
;; contextual-identity                  <SLOT>,<GENERIC>
;; contextual-identity-parent-uuid      <SLOT>,<GENERIC>
;; contextual-identity-uuid             <SLOT>,<GENERIC>
;; contextual-identity-uuid-byte-array  <SLOT>,<GENERIC>
;; contextual-identity-uuid-bit-vector  <SLOT>,<GENERIC>
;; contextual-identity-uuid-integer     <SLOT>,<GENERIC>
;; contextual-identity-uuid-string-36   <SLOT>,<GENERIC>
;; contextual-identity-uuid-version     <SLOT>,<GENERIC>
;;
(defclass contextual-object-uuid (base-uuid)
  ((contextual-identity
    :documentation  
    #.(format nil
              "An object identified by the `unicly:unique-universal-identifier'~%~
               per slot-value of contextual-identity-uuid."))
   (contextual-identity-parent-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier' which acts~%~
              as the NAMESPACE arg to `unicly:make-v5-uuid' in conjunction with~%~
              slot-value of contextual-identity as the NAME arg."))
   (contextual-identity-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier'.~%~@
               Value of this slot is suitable for use as a namespace argument to~%~@
               `unicly:make-v*-uuid'."))
   (contextual-identity-uuid-byte-array
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-16'.~%~@
               Value of this slot is the byte-array representation of the object in slot~%~@
               contextual-identity-uuid."))
   (contextual-identity-uuid-bit-vector
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-128'.~%~@
               Value of this slot is the bit-vector representation of the object in slot~%~@
               contextual-identity-uuid."))
   (contextual-identity-uuid-integer
    :documentation  
    #.(format nil
              "An object of type `unicly::uuid-integer-128'.~%~@
               Value of this slot is the 128bit integer representation of the object in slot~%~@
               contextual-identity-uuid."))
   (contextual-identity-uuid-string-36
    :documentation  
    #.(format nil
              "An object of type `unicly::uuid-hex-string-36'.~%~@
               Value of this slot is the hecadecimal integer representation of the object in slot~%~@
               contextual-identity-uuid."))
   (contextual-identity-uuid-version
    :documentation 
    #.(format nil 
              "The UUID version of the uuid namespace object in slot~%~@
               contextual-identity-uuid.")))
  (:documentation 
   #.(format nil
             "Instances of this class hold namespace metadata for classes whose instances~%~@
              share a common UUID namespace."))) 

(defclass identified-node (contextual-object-uuid w:node)
  ())

(defmethod initialize-instance :after ((node identified-node) &key base-namespace control-id)
  (update-contextual-object-uuid node
    :base-namespace (or base-namespace unicly:*uuid-namespace-url*)
    :control-id (or control-id (w:node-uri node)))
  (let ((uuid (contextual-identity-uuid node))
         (slots (mapcar #'sb-mop:slot-definition-name
                  (sb-mop:class-slots (find-class 'unicly:unique-universal-identifier)))))
    (dolist (slot slots)
      (setf (slot-value node slot) (slot-value uuid slot)))
    (values node uuid)))



(defun _::a (context node)
  (let* ((namestring (typecase node
                      (w:node (w:node-uri node))
                      (string node)))
          (contextual-node (make-instance '_::node :uri namestring)))
    (update-contextual-object-uuid contextual-node
      :base-namespace context :control-id namestring)))

(defmethod print-object ((o _::node) s)
  (print-object (w:node (w:node-uri o)) s))




;; mon:symbol-not-null-or-string-not-empty
;; mon::%fast-string-all-whitespace-p
(defun %verify-valid-string-or-symbol-for-identity (verify-identity)
  (declare #-:mon (type (or string (and symbol (not null))) verify-identity)
           #+:mon (type mon:symbol-not-null-or-string-not-empty verify-identity))
  #+:mon (unless (mon:symbol-not-null-or-string-not-empty-p verify-identity)
           (error "arg IDENTITY did not satisfy `mon:symbol-not-null-or-string-not-empty-p'"))
  #-:mon (when (and (stringp verify-identity)
                    (string= (make-string 0) verify-identity))
           (error "arg IDENTITY did not satisfy `mon:symbol-not-null-or-string-not-empty-p'"))
  (if (stringp verify-identity)
      (if #+:mon (mon::%fast-string-all-whitespace-p verify-identity)
          #-:mon (loop 
                    for char across verify-identity
                    always (member char (list #\SPACE #\NEWLINE #\TAB #\RETURN #\NO-BREAK_SPACE #\PAGE #\VT) :test 'char=))
          (error "arg IDENTITY must not be contained of all whitespace characters")
          verify-identity)
      verify-identity))

(defgeneric contextual-identity-parent-uuid (contextual-object)
  (:method  ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-parent-uuid)
      (slot-value contextual-object 'contextual-identity-parent-uuid)))
  (:documentation "Return the base-namespace UUID for CONTEXTUAL-OBJECT'S contextual-identity-parent-uuid slot-value."))

(defgeneric (setf contextual-identity-parent-uuid) (uuid contextual-object)
  (:method  ((uuid unicly:unique-universal-identifier) (contextual-object contextual-object-uuid))
    (setf (slot-value contextual-object 'contextual-identity-parent-uuid)
          uuid))
  (:documentation "Set UUID as CONTEXTUAL-OBJECT's contextual-identity-parent-uuid slot-value."))

(defgeneric contextual-identity-uuid-byte-array (contextual-object)
  (:method ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid-byte-array)
      (slot-value  contextual-object 'contextual-identity-uuid-byte-array)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity-uuid-byte-array slot-value."))

(defgeneric (setf contextual-identity-uuid-byte-array) (byte-array contextual-object)
  (:method ((byte-array array) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-byte-array-16 byte-array))
    (setf (slot-value  contextual-object 'contextual-identity-uuid-byte-array)
          byte-array))
  (:documentation "Set CONTEXTUAL-OBJECT's UUID namespace byte-array representation with BYTE-ARRAY.
BYTE-ARRAY is an contextual-object of type `unicly:uuid-byte-array-16'."))

(defgeneric contextual-identity-uuid-bit-vector (contextual-object)
  (:method ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid-bit-vector)
      (slot-value contextual-object 'contextual-identity-uuid-bit-vector)))
 (:documentation "Accessor for CONTEXTUAL-OBJECTs uuid namespace bit-vector representation."))

(defgeneric (setf contextual-identity-uuid-bit-vector) (bv contextual-object)

  (:method  ((uuid-bit-vector bit-vector) (contextual-object contextual-object-uuid)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (setf (slot-value contextual-object 'contextual-identity-uuid-bit-vector)
          uuid-bit-vector))

  (:method :after
    ((uuid-bit-vector bit-vector) (contextual-object contextual-object-uuid)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((slot-value-if (contextual-identity-uuid-version contextual-object))
          (version-if    (unicly::uuid-version-bit-vector uuid-bit-vector)))
      (declare ((mod 6) version-if))
      (when (zerop version-if)
        (error "Declining to set value for slot CONTEXTUAL-IDENTITY-UUID-VERSION ~
             to non-valid uuid version.~%~
             Likely the source UUID is corrupted or a null-uuid!~%~
             got bit-vector: ~S"
               uuid-bit-vector))
      (if (and slot-value-if (eql slot-value-if version-if))
          version-if
          (setf (contextual-identity-uuid-version contextual-object) version-if))))
  
  (:documentation "Set CONTEXTUAL-OBJECT's UUID namespace bit-vector representation with BV.
BV is an contextual-object of type `unicly:uuid-bit-vector-128'."))

(defgeneric contextual-identity-uuid-integer (contextual-object)
  (:method  ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid-integer)
      (slot-value contextual-object 'contextual-identity-uuid-integer)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity-uuid-integer slot-value."))

(defgeneric (setf contextual-identity-uuid-integer) (uuid-integer-128 contextual-object)
  (:method  ((uuid-integer-128 bignum) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-ub128 uuid-integer-128))
    (setf (slot-value contextual-object 'contextual-identity-uuid-integer)
          uuid-integer-128))
  (:documentation "Set CONTEXTUAL-OBJECT's 128bit integer representation with UUID-INTEGER-128."))

(defgeneric contextual-identity-uuid-string-36 (contextual-object)
  (:method  ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid-string-36)
    (slot-value contextual-object 'contextual-identity-uuid-string-36)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity-uuid-string-36 slot-value."))

(defgeneric (setf contextual-identity-uuid-string-36) (uuid-hex-string-36 contextual-object)
  (:method ((uuid-hex-string-36 string) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-hex-string-36 uuid-hex-string-36))
    (setf (slot-value contextual-object 'contextual-identity-uuid-string-36)
          uuid-hex-string-36))
  (:documentation "Set CONTEXTUAL-OBJECT's `unicly::uuid-hex-string-36' representation with UUID-HEX-STRING-36."))  

(defgeneric contextual-identity-uuid-version (contextual-object)
  (:method  ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid-version)
      (slot-value contextual-object 'contextual-identity-uuid-version)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity-uuid-version slot-value."))

(defgeneric contextual-object-uuid-description (contextual-object &key stream verbose)
  (:documentation "Print slot-values of CONTEXTUAL-OBJECT to STREAM."))

(defgeneric (setf contextual-identity-uuid-version) (bv-or-string contextual-object)

  ;; (:method  ((integer integer) (contextual-object contextual-object-uuid))
  (:method  ((fixnum fixnum) (contextual-object contextual-object-uuid))
    (declare ((mod 6) fixnum))
    (when (zerop fixnum)
      (error "Declining to set value for slot CONTEXTUAL-IDENTITY-UUID-VERSION ~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
    (setf (slot-value contextual-object 'contextual-identity-uuid-version)
          fixnum))

  (:method  ((uuid-bit-vector bit-vector) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((bv-version (unicly::uuid-version-bit-vector  uuid-bit-vector)))
      (declare ((mod 6) bv-version))
      (when (zerop bv-version)
        (error "Declining to set value for slot CONTEXTUAL-IDENTITY-UUID-VERSION~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
      (setf (slot-value contextual-object 'contextual-identity-uuid-version)
            (unicly::uuid-version-bit-vector  uuid-bit-vector))))

  (:documentation "Set CONTEXTUAL-OBJECT's uuid version with BV-OR-STRING.
BV-OR-STRING is either an contextual-object of type `unicly:uuid-bit-vector-128' or an
integer in the range [1,5]"))

(defgeneric contextual-identity (contextual-object)
  (:method((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity)
      (slot-value contextual-object 'contextual-identity)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity slot-value."))

;; (find-method #'(setf contextual-identity) '(:around) '(t contextual-object-uuid))
;; (remove-method #'(setf contextual-identity) (find-method #'(setf contextual-identity) '(:around) '(t contextual-object-uuid)))
(defgeneric (setf contextual-identity) (namespace-and-identity contextual-object)

  (:method  ((namespace-and-identity t) (contextual-object contextual-object-uuid))
    (setf (slot-value contextual-object 'contextual-identity)
          (cadr namespace-and-identity)))
  
  ;; :NOTE The NAMESPACE-AND-IDENTITY arg to the interface function
  ;; `make-contextual-object-uuid's contains the NAME arg for unicly:make-v5-uuid and
  ;; we want to store this as the slot-value of contextual-identity in order that we
  ;; may chase upwardly the class uuid's their namespaces and the parent
  ;; namespaces they descend from. To get the NAME into the slot-value of
  ;; contextual-identity we run an :around method which attempts to rollback in the
  ;; event of a failure (e.g. when the UUID representation for an arg ispoorly
  ;; configurued or otherwise illegitimate.
  (:method :around
    ((namespace-and-identity t) (contextual-object contextual-object-uuid))
    (declare (type list namespace-and-identity))
    (destructuring-bind (namespace identity) namespace-and-identity ;; (list (make-v4-uuid)  "<IDENTITY>")
      (declare ((and (or string symbol) (not null)) identity)
               (unicly:unique-universal-identifier namespace))
      (%verify-valid-string-or-symbol-for-identity identity)
      (let ((new-namespace       (unicly:make-v5-uuid namespace (if (symbolp identity) 
                                                                    (string identity)
                                                                    identity)))
            (old-id-slot         (contextual-identity                 contextual-object))
            (old-base-namespace  (contextual-identity-parent-uuid     contextual-object))
            ;; We might not have any slots set or only some, so get all of them.
            (old-namespace-slot  (contextual-identity-uuid            contextual-object))
            (old-byte-array-slot (contextual-identity-uuid-byte-array contextual-object))
            (old-bit-vector-slot (contextual-identity-uuid-bit-vector contextual-object))
            (old-integer-slot    (contextual-identity-uuid-integer    contextual-object))
            (old-hex-string-slot (contextual-identity-uuid-string-36  contextual-object))
            ;; (old-version-slot    (contextual-identity-uuid-version     contextual-object))
            (new-namespace-slot  '()))
        (unwind-protect 
             (progn 
               (setf new-namespace-slot 
                     (ignore-errors 
                       (setf (contextual-identity-uuid contextual-object)
                             new-namespace)))
               ;; If we didn't error we can safeley set the base-namespace slot
               ;; else unset what we just set...
               (when new-namespace-slot 
                 (setf (contextual-identity-parent-uuid contextual-object)
                       namespace)
                 (call-next-method)))
          (unless new-namespace-slot
            (let ((slot-val-for-rollback (or old-bit-vector-slot
                                             old-namespace-slot
                                             old-byte-array-slot
                                             old-integer-slot
                                             old-hex-string-slot)))
              ;; Following form will clear the existing contextual-identity and
              ;; contextual-identity-parent-uuid slots with slot-makunbound. 
              ;; We set them back to their previous state afterwards.
              (when slot-val-for-rollback
                (setf (contextual-identity-parent-uuid contextual-object)
                      slot-val-for-rollback))
              (if (and old-base-namespace old-id-slot)
                  (progn
                    (setf (contextual-identity-parent-uuid contextual-object) old-base-namespace)
                    ;; We set the slot-value explicitly instead of using the method
                    ;; specialized because it would land us right back here!
                    (setf (slot-value contextual-object 'contextual-object-uuid-identity) old-id-slot))
                  ;; If either the control-identity or base-namespace slots is
                  ;; null or unbound the the other should be as well.
                  (progn 
                    (slot-makunbound contextual-object 'contextual-identity-parent-uuid)
                    (slot-makunbound contextual-object 'contextual-identity)))))))))

  (:documentation "Set CONTEXTUAL-OBJECT's contextual-identity slot-value to IDENTITY"))

(defgeneric contextual-identity-uuid (contextual-object)
  (:method  ((contextual-object contextual-object-uuid))
    (when (slot-boundp contextual-object 'contextual-identity-uuid)
      (slot-value contextual-object 'contextual-identity-uuid)))
  (:documentation "Accessor for CONTEXTUAL-OBJECT's contextual-identity-uuid slot-value."))

(defgeneric (setf contextual-identity-uuid) (coercable-uuid contextual-object)

  ;; :NOTE `unicly:make-uuid-from-string' already coerces a uuid contextual-object with
  ;; `unicly:uuid-copy-uuid' we keep the method dispatch b/c we can check string
  ;; validity earlier.
  (:method  ((uuid-string string) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-hex-string-36 uuid-string))
    (let ((uuid-from-string (unicly:make-uuid-from-string uuid-string)))
      (declare (unicly:unique-universal-identifier uuid-from-string))
      (setf (slot-value contextual-object 'contextual-identity-uuid)
            uuid-from-string)))

  (:method  ((uuid unicly:unique-universal-identifier) (contextual-object contextual-object-uuid)) 
    (setf (slot-value contextual-object 'contextual-identity-uuid)
          (uuid-copy-uuid uuid)))

  (:method  ((uuid-bit-vector bit-vector) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((uuid-from-bv (unicly::uuid-from-bit-vector uuid-bit-vector)))
      (declare (unicly:unique-universal-identifier uuid-from-bv))
      (setf (slot-value contextual-object 'contextual-identity-uuid)
            uuid-from-bv)))

  (:method  ((uuid-byte-array array) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-byte-array-16 uuid-byte-array))
    (let ((uuid-from-byte-array (uuid-from-byte-array uuid-byte-array)))
      (declare (unicly:unique-universal-identifier uuid-from-byte-array))
      (setf (slot-value contextual-object 'contextual-identity-uuid)
            uuid-from-byte-array)))

  (:method  ((integer-128 bignum) (contextual-object contextual-object-uuid))
    (declare (unicly::uuid-ub128 integer-128))
    (let ((uuid-from-int (unicly::uuid-from-bit-vector 
                          (unicly::uuid-integer-128-to-bit-vector integer-128))))
      (declare (unicly:unique-universal-identifier uuid-from-int))
      (setf (slot-value contextual-object 'contextual-identity-uuid)
            uuid-from-int)))

  ;; The :after method helps us make sure all other slots get propagated whenever
  ;; a slot containing a uuid representation gets touched. 
  ;; This was an early 
  ;; (find-method #'(setf contextual-identity-uuid) '(:after) '(t contextual-object-uuid))
  (:method  :after 
    ((uuid-arg t) (contextual-object contextual-object-uuid))
    (let* ((uuid-bv-prevent (unicly:uuid-to-bit-vector 
                             (contextual-identity-uuid contextual-object)))
           (uuid-bv    (if (unicly:uuid-bit-vector-128-p uuid-arg)
                           ;; if the uuid-arg is the same bv128 as the conversion don't re-trigger
                           (if (unicly:uuid-bit-vector-eql uuid-arg uuid-bv-prevent)
                               nil
                               uuid-arg)
                           uuid-bv-prevent))
           (uuid-bytes (if (unicly:uuid-byte-array-16-p uuid-arg)
                           (let* ((existing-slot    
                                   (contextual-identity-uuid-byte-array contextual-object))
                                  (existing-slot-bv 
                                   (when existing-slot 
                                     (unicly:uuid-byte-array-to-bit-vector existing-slot)))
                                  (ba-to-bv 
                                   (if existing-slot-bv
                                       nil
                                       (unicly:uuid-byte-array-to-bit-vector uuid-arg))))
                             (if existing-slot-bv
                                 (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)
                                 (if (unicly:uuid-bit-vector-eql ba-to-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)))
                           (unicly:uuid-bit-vector-to-byte-array uuid-bv-prevent)))
           (uuid-int   (if (typep uuid-arg 'bignum)
                           (let* ((existing-slot    
                                   (contextual-identity-uuid-integer contextual-object))
                                  (existing-slot-bv
                                   (when existing-slot 
                                     (unicly::uuid-integer-128-to-bit-vector existing-slot)))
                                  (int-to-bv      
                                   (if existing-slot-bv
                                       nil
                                       (unicly::uuid-integer-128-to-bit-vector uuid-arg))))
                             (if existing-slot-bv
                                 (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)
                                 (if (unicly:uuid-bit-vector-eql int-to-bv uuid-bv-prevent)
                                     nil
                                     uuid-arg)))
                           (unicly::uuid-bit-vector-to-integer uuid-bv-prevent)))
           (uuid-hex-36   (if (and (stringp uuid-arg)
                                   (unicly::uuid-string-36-p (the string uuid-arg)))
                              (let* ((existing-slot
                                      (contextual-identity-uuid-string-36 contextual-object))
                                     (existing-slot-bv 
                                      (when existing-slot 
                                        (unicly::uuid-to-bit-vector (unicly:make-uuid-from-string existing-slot))))
                                     (hex-to-bv         
                                      (if existing-slot-bv
                                          nil
                                          (unicly::uuid-to-bit-vector (unicly:make-uuid-from-string uuid-arg)))))
                                (if existing-slot-bv
                                    (if (unicly:uuid-bit-vector-eql existing-slot-bv uuid-bv-prevent)
                                        nil
                                        uuid-arg)
                                    (if (unicly:uuid-bit-vector-eql hex-to-bv uuid-bv-prevent)
                                        nil
                                        uuid-arg)))
                              (unicly:uuid-princ-to-string (contextual-identity-uuid contextual-object)))))
      (declare (type (or unicly::uuid-byte-array-16 null)  uuid-bytes)
               (type (or unicly::uuid-bit-vector-128 null) uuid-bv)
               (type (or unicly::uuid-ub128 null)          uuid-int)
               (type (or unicly::uuid-hex-string-36 null)  uuid-hex-36))
      (when uuid-bv
        (setf (contextual-identity-uuid-bit-vector contextual-object) uuid-bv))
      (when uuid-bytes
        (setf (contextual-identity-uuid-byte-array contextual-object) uuid-bytes))
      (when uuid-hex-36
        (setf (contextual-identity-uuid-string-36  contextual-object) uuid-hex-36))
      (when uuid-int
        (setf (contextual-identity-uuid-integer    contextual-object) uuid-int))
      (if (contextual-identity contextual-object)
          (if (contextual-identity-parent-uuid contextual-object)
              ;; make sure that the namespace and identity evaluate to the
              ;; namespace we just set, and if not remove them.
              (unless (unicly::uuid-bit-vector-eql
                       (contextual-identity-uuid-bit-vector contextual-object)
                       (uuid-to-bit-vector
                        (make-v5-uuid
                         (contextual-identity-parent-uuid contextual-object)
                         (string (contextual-identity contextual-object)))))
                (slot-makunbound contextual-object 'contextual-identity)
                (slot-makunbound contextual-object 'contextual-identity-parent-uuid))
              (slot-makunbound contextual-object 'contextual-identity))
          ;; The control-identity isn't present, so if the base-namespace is as
          ;; well it shouldn't be
          (when (contextual-identity-parent-uuid contextual-object)
            (slot-makunbound contextual-object 'contextual-identity-parent-uuid)))
      uuid-arg))

  (:documentation "Set uuid namespace for CONTEXTUAL-OBJECT with COERCABLE-UUID
COERCABLE-UUID is a representation of a Unicly UUID in some form, e.g.:
 hex-string-36, byte-array-16, bit-vector-128, unique-universal-identifier"))

;; namespace-and-identity
(defun update-contextual-object-uuid (contextual-object &key base-namespace control-id)
  (declare (type contextual-object-uuid contextual-object)
           (type unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (contextual-identity contextual-object) (list base-namespace control-id))
    (setf (contextual-identity-uuid contextual-object) new-nmspc))
  contextual-object)

(defun make-contextual-object-uuid (&key base-namespace control-id)
  (declare (unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-obj   (make-instance 'contextual-object-uuid))
        (new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (contextual-identity new-obj) (list base-namespace control-id))
    (setf (contextual-identity-uuid new-obj) new-nmspc)
    new-obj))


;; (find-method #'contextual-object-uuid-description nil '(contextual-object-uuid))
(defmethod contextual-object-uuid-description ((contextual-object contextual-object-uuid) &key stream verbose)
  (declare (type boolean verbose))
  (if (not verbose)
      (let* ((unbound "#<UNBOUND>")
             (contextual-id-if  (contextual-identity contextual-object))
             (contextual-id     (if contextual-id-if
                             (prin1-to-string contextual-id-if)
                             unbound))
             (contextual-id-uuid   (or (contextual-identity-uuid contextual-object)
                                unbound)))
        (with-standard-io-syntax 
          (format stream "~%~{~4T~29A~A~^~%~}~4T"
                  (list 
                   ;; "TYPE-OF:"    (type-of contextual-object)
                   ":CONTEXTUAL-IDENTITY"          contextual-id
                   ":CONTEXTUAL-IDENTITY-UUID"     contextual-id-uuid))))

      (let* ((unbound "#<UNBOUND>")
             (contextual-id-if  (contextual-identity contextual-object))
             (contextual-id     (if contextual-id-if
                             (prin1-to-string contextual-id-if)
                             unbound))
             (contextual-id-uuid   (or (contextual-identity-uuid contextual-object)
                                unbound))
             (byte-array    (or (contextual-identity-uuid-byte-array contextual-object)
                                unbound))
             (bit-vector    (or (contextual-identity-uuid-bit-vector contextual-object)
                                unbound))
             (hex-string-if (contextual-identity-uuid-string-36 contextual-object))
             (hex-string    (if hex-string-if
                                (prin1-to-string hex-string-if)
                                unbound))
             (integer-128-if (contextual-identity-uuid-integer contextual-object))
             (integer-128    (if integer-128-if
                                 (let ((*print-base* 16)
                                       (*print-radix* t)) 
                                   (princ-to-string integer-128-if))
                                 unbound))
             (parent-uuid    (or (contextual-identity-parent-uuid contextual-object)
                                 unbound))         
             (version-if    (contextual-identity-uuid-version contextual-object))
             (version       (if version-if 
                                (prin1-to-string version-if)
                                unbound))
             (format-description
              (let ((*print-lines* 0))
                (format nil "~%~{~4T~40A~A~%~}"
                        (list ;; "TYPE-OF:"    (type-of contextual-object)
                         ":CONTEXTUAL-IDENTITY"                  contextual-id
                         ":CONTEXTUAL-IDENTITY-UUID"             contextual-id-uuid
                         ":CONTEXTUAL-IDENTITY-PARENT-UUID"      parent-uuid
                         ":CONTEXTUAL-IDENTITY-UUID-STRING-36"   hex-string
                         ":CONTEXTUAL-IDENTITY-UUID-BYTE-ARRAY"  byte-array
                         ":CONTEXTUAL-IDENTITY-UUID-INTEGER"     integer-128
                         ":CONTEXTUAL-IDENTITY-UUID-BIT-VECTOR"  bit-vector
                         ":CONTEXTUAL-IDENTITY-UUID-VERSION"     version)))))
        (with-standard-io-syntax 
          (princ format-description stream)))))

(defmethod describe-object ((contextual-object contextual-object-uuid) stream)
  (print (type-of contextual-object) stream)
  (contextual-object-uuid-description contextual-object :stream stream :verbose t))

(defmethod print-object ((contextual-object contextual-object-uuid) stream)
  (print-unreadable-object (contextual-object stream :type t) ;; :identity t)
    (contextual-object-uuid-description contextual-object :stream  stream)))
