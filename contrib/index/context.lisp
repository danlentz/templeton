;;; :FILE-CREATED <Timestamp: #{2011-05-23T20:18:33-04:00Z}#{11211} - by MON>
;;; :FILE dbc-class-uuid-namespace-for-control-id.lisp
;;; ==============================
;;(delete-package :id)
(defpackage :templeton.context
  (:nicknames :id)
  (:use :unicly :common-lisp)
  (:export
    :context-object-uuid
    :context-identity
    :context-identity-parent-uuid
    :context-identity-uuid
    :context-identity-uuid-byte-array
    :context-identity-uuid-bit-vector
    :context-identity-uuid-integer
    :context-identity-uuid-string-36
    :context-identity-uuid-version
    :context-object-uuid-description
    :make-context-object-uuid))

(in-package :id)
;;; ==============================
;;
#|
(define-symbol-macro bubba#
  (defparameter _::bubba#
    (make-context-object-uuid :base-namespace unicly:*uuid-namespace-url*
      :control-id "bubba")))
*TT--CONTEXT-OBJ-UUID*
|#
;; => *TT--CONTEXT-OBJ-UUID*
;;
;; *TT--CONTEXT-OBJ-UUID*
;; |=> #<CONTEXT-OBJECT-UUID 
;; |     CONTEXT-ID:      "bubba"
;; |     CONTEXT-UUID:    eea1105e-3681-5117-99b6-7b2b5fe1f3c7>
;;
;; (unicly:uuid-eql (make-v5-uuid (context-identity-parent-uuid  *tt--context-obj-uuid*)
;;                                (context-identity  *tt--context-obj-uuid*))
;;                  (context-identity-uuid *tt--context-obj-uuid*))
;; => T
;;
;; (setf *tt--context-obj-uuid*
;;       (make-context-object-uuid :base-namespace unicly:*uuid-namespace-dns* 
;;                                :control-id 'bubba))
;; |=> #<CONTEXT-OBJECT-UUID 
;; |     CONTEXT-ID:      BUBBA
;; |     CONTEXT-UUID:    60ada823-d6de-5729-9e7e-8e44a57e400d >
;;
;; (unicly:uuid-eql (make-v5-uuid (context-identity-parent-uuid  *tt--context-obj-uuid*)
;;                                 (string (context-identity  *tt--context-obj-uuid*)))
;;                   (context-identity-uuid *tt--context-obj-uuid*))
;; => T
;;
;; (setf (context-identity *tt--context-obj-uuid*)
;;       (list (make-v4-uuid) "new-bubba"))
;; => "new-bubba"
;;
;; *tt--context-obj-uuid*
;;  |=> #<CONTEXT-OBJECT-UUID 
;;  |    :CONTEXT-IDENTITY             "new-bubba"
;;  |    :CONTEXT-IDENTITY-UUID        0ef86c6f-b263-5987-a90c-3fcca581bc38 >
;;
;; (context-identity-uuid  *tt--context-obj-uuid*)
;; ; => 0ef86c6f-b263-5987-a90c-3fcca581bc38
;;
;; (context-identity  *tt--context-obj-uuid*)
;;  ;=> "new-bubba"
;;
;; (context-identity-parent-uuid  *tt--context-obj-uuid*)
;;  ;=> addedf68-81a4-47cd-a7b1-b96779f8b676
;; 
;; (type-of (context-identity-parent-uuid  *tt--context-obj-uuid*))
;; => UNIQUE-UNIVERSAL-IDENTIFIER
;;
;; (unicly:uuid-version-uuid (context-identity-parent-uuid  *tt--context-obj-uuid*))
;; ;=> 4
;;
;; (context-identity-uuid-version *tt--context-obj-uuid*)
;; ;=> 5
;;
;; (context-identity-uuid-byte-array  *tt--context-obj-uuid*)
;; ;=> #(14 248 108 111 178 99 89 135 169 12 63 204 165 129 188 56)
;;
;; (context-identity-uuid-bit-vector  *tt--context-obj-uuid*)
;; ;=> #*000011101111100001101100011011111011001001100011010 ...
;;
;; (context-identity-uuid-integer     *tt--context-obj-uuid*)
;; ;=> 19899080911677131133725998230922181688
;;
;; (context-identity-uuid-string-36   *tt--context-obj-uuid*)
;; ;=> "0ef86c6f-b263-5987-a90c-3fcca581bc38"
;;
;; (update-context-object-uuid *tt--context-obj-uuid*
;;                            :base-namespace (make-v5-uuid *uuid-namespace-dns* (string '*tt--context-obj-uuid*))
;;                            :control-id '*tt--context-obj-uuid*)
;; |=> #<CONTEXT-OBJECT-UUID 
;; |    :CONTEXT-IDENTITY             *TT--CONTEXT-OBJ-UUID*
;; |    :CONTEXT-IDENTITY-UUID        db197774-6955-55b1-ac30-143864977f41 >
;;
;;
;; (context-identity *TT--CONTEXT-OBJ-UUID*)
;; ;=> *TT--CONTEXT-OBJ-UUID*
;;
;; (eq (context-identity *TT--CONTEXT-OBJ-UUID*) '*TT--CONTEXT-OBJ-UUID*)
;; ;=> T
;;
;;; ==============================


;; *package*

(defclass base-uuid (unicly:unique-universal-identifier)())

;;;; The class CONTEXT-OBJECT-UUID
;;
;; context-object-uuid               <CLASS>
;; context-identity                  <SLOT>,<GENERIC>
;; context-identity-parent-uuid      <SLOT>,<GENERIC>
;; context-identity-uuid             <SLOT>,<GENERIC>
;; context-identity-uuid-byte-array  <SLOT>,<GENERIC>
;; context-identity-uuid-bit-vector  <SLOT>,<GENERIC>
;; context-identity-uuid-integer     <SLOT>,<GENERIC>
;; context-identity-uuid-string-36   <SLOT>,<GENERIC>
;; context-identity-uuid-version     <SLOT>,<GENERIC>
;;
(defclass context-object-uuid (base-uuid)
  ((context-identity
    :documentation  
    #.(format nil
              "An object identified by the `unicly:unique-universal-identifier'~%~
               per slot-value of context-identity-uuid."))
   (context-identity-parent-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier' which acts~%~
              as the NAMESPACE arg to `unicly:make-v5-uuid' in conjunction with~%~
              slot-value of context-identity as the NAME arg."))
   (context-identity-uuid
    :documentation  
    #.(format nil
              "An object of type `unicly:unique-universal-identifier'.~%~@
               Value of this slot is suitable for use as a namespace argument to~%~@
               `unicly:make-v*-uuid'."))
   (context-identity-uuid-byte-array
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-16'.~%~@
               Value of this slot is the byte-array representation of the object in slot~%~@
               context-identity-uuid."))
   (context-identity-uuid-bit-vector
    :documentation  
    #.(format nil
              "An object of type `unicly:uuid-byte-array-128'.~%~@
               Value of this slot is the bit-vector representation of the object in slot~%~@
               context-identity-uuid."))
   (context-identity-uuid-integer
    :documentation  
    #.(format nil
              "An object of type `unicly::uuid-integer-128'.~%~@
               Value of this slot is the 128bit integer representation of the object in slot~%~@
               context-identity-uuid."))
   (context-identity-uuid-string-36
    :documentation  
    #.(format nil
              "An object of type `unicly::uuid-hex-string-36'.~%~@
               Value of this slot is the hecadecimal integer representation of the object in slot~%~@
               context-identity-uuid."))
   (context-identity-uuid-version
    :documentation 
    #.(format nil 
              "The UUID version of the uuid namespace object in slot~%~@
               context-identity-uuid.")))
  (:documentation 
   #.(format nil
             "Instances of this class hold namespace metadata for classes whose instances~%~@
              share a common UUID namespace."))) 


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

(defgeneric context-identity-parent-uuid (context-object)
  (:method  ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-parent-uuid)
      (slot-value context-object 'context-identity-parent-uuid)))
  (:documentation "Return the base-namespace UUID for CONTEXT-OBJECT'S context-identity-parent-uuid slot-value."))

(defgeneric (setf context-identity-parent-uuid) (uuid context-object)
  (:method  ((uuid unicly:unique-universal-identifier) (context-object context-object-uuid))
    (setf (slot-value context-object 'context-identity-parent-uuid)
          uuid))
  (:documentation "Set UUID as CONTEXT-OBJECT's context-identity-parent-uuid slot-value."))

(defgeneric context-identity-uuid-byte-array (context-object)
  (:method ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid-byte-array)
      (slot-value  context-object 'context-identity-uuid-byte-array)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity-uuid-byte-array slot-value."))

(defgeneric (setf context-identity-uuid-byte-array) (byte-array context-object)
  (:method ((byte-array array) (context-object context-object-uuid))
    (declare (unicly::uuid-byte-array-16 byte-array))
    (setf (slot-value  context-object 'context-identity-uuid-byte-array)
          byte-array))
  (:documentation "Set CONTEXT-OBJECT's UUID namespace byte-array representation with BYTE-ARRAY.
BYTE-ARRAY is an context-object of type `unicly:uuid-byte-array-16'."))

(defgeneric context-identity-uuid-bit-vector (context-object)
  (:method ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid-bit-vector)
      (slot-value context-object 'context-identity-uuid-bit-vector)))
 (:documentation "Accessor for CONTEXT-OBJECTs uuid namespace bit-vector representation."))

(defgeneric (setf context-identity-uuid-bit-vector) (bv context-object)

  (:method  ((uuid-bit-vector bit-vector) (context-object context-object-uuid)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (setf (slot-value context-object 'context-identity-uuid-bit-vector)
          uuid-bit-vector))

  (:method :after
    ((uuid-bit-vector bit-vector) (context-object context-object-uuid)) ; simple-bit-vector
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((slot-value-if (context-identity-uuid-version context-object))
          (version-if    (unicly::uuid-version-bit-vector uuid-bit-vector)))
      (declare ((mod 6) version-if))
      (when (zerop version-if)
        (error "Declining to set value for slot CONTEXT-IDENTITY-UUID-VERSION ~
             to non-valid uuid version.~%~
             Likely the source UUID is corrupted or a null-uuid!~%~
             got bit-vector: ~S"
               uuid-bit-vector))
      (if (and slot-value-if (eql slot-value-if version-if))
          version-if
          (setf (context-identity-uuid-version context-object) version-if))))
  
  (:documentation "Set CONTEXT-OBJECT's UUID namespace bit-vector representation with BV.
BV is an context-object of type `unicly:uuid-bit-vector-128'."))

(defgeneric context-identity-uuid-integer (context-object)
  (:method  ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid-integer)
      (slot-value context-object 'context-identity-uuid-integer)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity-uuid-integer slot-value."))

(defgeneric (setf context-identity-uuid-integer) (uuid-integer-128 context-object)
  (:method  ((uuid-integer-128 bignum) (context-object context-object-uuid))
    (declare (unicly::uuid-ub128 uuid-integer-128))
    (setf (slot-value context-object 'context-identity-uuid-integer)
          uuid-integer-128))
  (:documentation "Set CONTEXT-OBJECT's 128bit integer representation with UUID-INTEGER-128."))

(defgeneric context-identity-uuid-string-36 (context-object)
  (:method  ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid-string-36)
    (slot-value context-object 'context-identity-uuid-string-36)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity-uuid-string-36 slot-value."))

(defgeneric (setf context-identity-uuid-string-36) (uuid-hex-string-36 context-object)
  (:method ((uuid-hex-string-36 string) (context-object context-object-uuid))
    (declare (unicly::uuid-hex-string-36 uuid-hex-string-36))
    (setf (slot-value context-object 'context-identity-uuid-string-36)
          uuid-hex-string-36))
  (:documentation "Set CONTEXT-OBJECT's `unicly::uuid-hex-string-36' representation with UUID-HEX-STRING-36."))  

(defgeneric context-identity-uuid-version (context-object)
  (:method  ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid-version)
      (slot-value context-object 'context-identity-uuid-version)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity-uuid-version slot-value."))

(defgeneric context-object-uuid-description (context-object &key stream verbose)
  (:documentation "Print slot-values of CONTEXT-OBJECT to STREAM."))

(defgeneric (setf context-identity-uuid-version) (bv-or-string context-object)

  ;; (:method  ((integer integer) (context-object context-object-uuid))
  (:method  ((fixnum fixnum) (context-object context-object-uuid))
    (declare ((mod 6) fixnum))
    (when (zerop fixnum)
      (error "Declining to set value for slot CONTEXT-IDENTITY-UUID-VERSION ~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
    (setf (slot-value context-object 'context-identity-uuid-version)
          fixnum))

  (:method  ((uuid-bit-vector bit-vector) (context-object context-object-uuid))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((bv-version (unicly::uuid-version-bit-vector  uuid-bit-vector)))
      (declare ((mod 6) bv-version))
      (when (zerop bv-version)
        (error "Declining to set value for slot CONTEXT-IDENTITY-UUID-VERSION~
            to non-valid uuid version. Likely the source UUID is corrupted!"))
      (setf (slot-value context-object 'context-identity-uuid-version)
            (unicly::uuid-version-bit-vector  uuid-bit-vector))))

  (:documentation "Set CONTEXT-OBJECT's uuid version with BV-OR-STRING.
BV-OR-STRING is either an context-object of type `unicly:uuid-bit-vector-128' or an
integer in the range [1,5]"))

(defgeneric context-identity (context-object)
  (:method((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity)
      (slot-value context-object 'context-identity)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity slot-value."))

;; (find-method #'(setf context-identity) '(:around) '(t context-object-uuid))
;; (remove-method #'(setf context-identity) (find-method #'(setf context-identity) '(:around) '(t context-object-uuid)))
(defgeneric (setf context-identity) (namespace-and-identity context-object)

  (:method  ((namespace-and-identity t) (context-object context-object-uuid))
    (setf (slot-value context-object 'context-identity)
          (cadr namespace-and-identity)))
  
  ;; :NOTE The NAMESPACE-AND-IDENTITY arg to the interface function
  ;; `make-context-object-uuid's contains the NAME arg for unicly:make-v5-uuid and
  ;; we want to store this as the slot-value of context-identity in order that we
  ;; may chase upwardly the class uuid's their namespaces and the parent
  ;; namespaces they descend from. To get the NAME into the slot-value of
  ;; context-identity we run an :around method which attempts to rollback in the
  ;; event of a failure (e.g. when the UUID representation for an arg ispoorly
  ;; configurued or otherwise illegitimate.
  (:method :around
    ((namespace-and-identity t) (context-object context-object-uuid))
    (declare (type list namespace-and-identity))
    (destructuring-bind (namespace identity) namespace-and-identity ;; (list (make-v4-uuid)  "<IDENTITY>")
      (declare ((and (or string symbol) (not null)) identity)
               (unicly:unique-universal-identifier namespace))
      (%verify-valid-string-or-symbol-for-identity identity)
      (let ((new-namespace       (unicly:make-v5-uuid namespace (if (symbolp identity) 
                                                                    (string identity)
                                                                    identity)))
            (old-id-slot         (context-identity                 context-object))
            (old-base-namespace  (context-identity-parent-uuid     context-object))
            ;; We might not have any slots set or only some, so get all of them.
            (old-namespace-slot  (context-identity-uuid            context-object))
            (old-byte-array-slot (context-identity-uuid-byte-array context-object))
            (old-bit-vector-slot (context-identity-uuid-bit-vector context-object))
            (old-integer-slot    (context-identity-uuid-integer    context-object))
            (old-hex-string-slot (context-identity-uuid-string-36  context-object))
            ;; (old-version-slot    (context-identity-uuid-version     context-object))
            (new-namespace-slot  '()))
        (unwind-protect 
             (progn 
               (setf new-namespace-slot 
                     (ignore-errors 
                       (setf (context-identity-uuid context-object)
                             new-namespace)))
               ;; If we didn't error we can safeley set the base-namespace slot
               ;; else unset what we just set...
               (when new-namespace-slot 
                 (setf (context-identity-parent-uuid context-object)
                       namespace)
                 (call-next-method)))
          (unless new-namespace-slot
            (let ((slot-val-for-rollback (or old-bit-vector-slot
                                             old-namespace-slot
                                             old-byte-array-slot
                                             old-integer-slot
                                             old-hex-string-slot)))
              ;; Following form will clear the existing context-identity and
              ;; context-identity-parent-uuid slots with slot-makunbound. 
              ;; We set them back to their previous state afterwards.
              (when slot-val-for-rollback
                (setf (context-identity-parent-uuid context-object)
                      slot-val-for-rollback))
              (if (and old-base-namespace old-id-slot)
                  (progn
                    (setf (context-identity-parent-uuid context-object) old-base-namespace)
                    ;; We set the slot-value explicitly instead of using the method
                    ;; specialized because it would land us right back here!
                    (setf (slot-value context-object 'context-object-uuid-identity) old-id-slot))
                  ;; If either the control-identity or base-namespace slots is
                  ;; null or unbound the the other should be as well.
                  (progn 
                    (slot-makunbound context-object 'context-identity-parent-uuid)
                    (slot-makunbound context-object 'context-identity)))))))))

  (:documentation "Set CONTEXT-OBJECT's context-identity slot-value to IDENTITY"))

(defgeneric context-identity-uuid (context-object)
  (:method  ((context-object context-object-uuid))
    (when (slot-boundp context-object 'context-identity-uuid)
      (slot-value context-object 'context-identity-uuid)))
  (:documentation "Accessor for CONTEXT-OBJECT's context-identity-uuid slot-value."))

(defgeneric (setf context-identity-uuid) (coercable-uuid context-object)

  ;; :NOTE `unicly:make-uuid-from-string' already coerces a uuid context-object with
  ;; `unicly:uuid-copy-uuid' we keep the method dispatch b/c we can check string
  ;; validity earlier.
  (:method  ((uuid-string string) (context-object context-object-uuid))
    (declare (unicly::uuid-hex-string-36 uuid-string))
    (let ((uuid-from-string (unicly:make-uuid-from-string uuid-string)))
      (declare (unicly:unique-universal-identifier uuid-from-string))
      (setf (slot-value context-object 'context-identity-uuid)
            uuid-from-string)))

  (:method  ((uuid unicly:unique-universal-identifier) (context-object context-object-uuid)) 
    (setf (slot-value context-object 'context-identity-uuid)
          (uuid-copy-uuid uuid)))

  (:method  ((uuid-bit-vector bit-vector) (context-object context-object-uuid))
    (declare (unicly::uuid-bit-vector-128 uuid-bit-vector))
    (let ((uuid-from-bv (unicly::uuid-from-bit-vector uuid-bit-vector)))
      (declare (unicly:unique-universal-identifier uuid-from-bv))
      (setf (slot-value context-object 'context-identity-uuid)
            uuid-from-bv)))

  (:method  ((uuid-byte-array array) (context-object context-object-uuid))
    (declare (unicly::uuid-byte-array-16 uuid-byte-array))
    (let ((uuid-from-byte-array (uuid-from-byte-array uuid-byte-array)))
      (declare (unicly:unique-universal-identifier uuid-from-byte-array))
      (setf (slot-value context-object 'context-identity-uuid)
            uuid-from-byte-array)))

  (:method  ((integer-128 bignum) (context-object context-object-uuid))
    (declare (unicly::uuid-ub128 integer-128))
    (let ((uuid-from-int (unicly::uuid-from-bit-vector 
                          (unicly::uuid-integer-128-to-bit-vector integer-128))))
      (declare (unicly:unique-universal-identifier uuid-from-int))
      (setf (slot-value context-object 'context-identity-uuid)
            uuid-from-int)))

  ;; The :after method helps us make sure all other slots get propagated whenever
  ;; a slot containing a uuid representation gets touched. 
  ;; This was an early 
  ;; (find-method #'(setf context-identity-uuid) '(:after) '(t context-object-uuid))
  (:method  :after 
    ((uuid-arg t) (context-object context-object-uuid))
    (let* ((uuid-bv-prevent (unicly:uuid-to-bit-vector 
                             (context-identity-uuid context-object)))
           (uuid-bv    (if (unicly:uuid-bit-vector-128-p uuid-arg)
                           ;; if the uuid-arg is the same bv128 as the conversion don't re-trigger
                           (if (unicly:uuid-bit-vector-eql uuid-arg uuid-bv-prevent)
                               nil
                               uuid-arg)
                           uuid-bv-prevent))
           (uuid-bytes (if (unicly:uuid-byte-array-16-p uuid-arg)
                           (let* ((existing-slot    
                                   (context-identity-uuid-byte-array context-object))
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
                                   (context-identity-uuid-integer context-object))
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
                                      (context-identity-uuid-string-36 context-object))
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
                              (unicly:uuid-princ-to-string (context-identity-uuid context-object)))))
      (declare (type (or unicly::uuid-byte-array-16 null)  uuid-bytes)
               (type (or unicly::uuid-bit-vector-128 null) uuid-bv)
               (type (or unicly::uuid-ub128 null)          uuid-int)
               (type (or unicly::uuid-hex-string-36 null)  uuid-hex-36))
      (when uuid-bv
        (setf (context-identity-uuid-bit-vector context-object) uuid-bv))
      (when uuid-bytes
        (setf (context-identity-uuid-byte-array context-object) uuid-bytes))
      (when uuid-hex-36
        (setf (context-identity-uuid-string-36  context-object) uuid-hex-36))
      (when uuid-int
        (setf (context-identity-uuid-integer    context-object) uuid-int))
      (if (context-identity context-object)
          (if (context-identity-parent-uuid context-object)
              ;; make sure that the namespace and identity evaluate to the
              ;; namespace we just set, and if not remove them.
              (unless (unicly::uuid-bit-vector-eql
                       (context-identity-uuid-bit-vector context-object)
                       (uuid-to-bit-vector
                        (make-v5-uuid
                         (context-identity-parent-uuid context-object)
                         (string (context-identity context-object)))))
                (slot-makunbound context-object 'context-identity)
                (slot-makunbound context-object 'context-identity-parent-uuid))
              (slot-makunbound context-object 'context-identity))
          ;; The control-identity isn't present, so if the base-namespace is as
          ;; well it shouldn't be
          (when (context-identity-parent-uuid context-object)
            (slot-makunbound context-object 'context-identity-parent-uuid)))
      uuid-arg))

  (:documentation "Set uuid namespace for CONTEXT-OBJECT with COERCABLE-UUID
COERCABLE-UUID is a representation of a Unicly UUID in some form, e.g.:
 hex-string-36, byte-array-16, bit-vector-128, unique-universal-identifier"))

;; namespace-and-identity
(defun update-context-object-uuid (context-object &key base-namespace control-id)
  (declare (type context-object-uuid context-object)
           (type unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (context-identity context-object) (list base-namespace control-id))
    (setf (context-identity-uuid context-object) new-nmspc))
  context-object)

(defun make-context-object-uuid (&key base-namespace control-id)
  (declare (unicly:unique-universal-identifier base-namespace)
           #-:mon (type (or string (and symbol (not null))) control-id)
           #+:mon ((or mon:string-not-null-empty-or-all-whitespace mon:symbol-not-null)
                   control-id))
  ;; #-:mon (%verify-valid-string-or-symbol-for-identity identity)
  (let ((new-obj   (make-instance 'context-object-uuid))
        (new-nmspc (unicly:make-v5-uuid base-namespace 
                                        (if (symbolp control-id) 
                                            (string control-id)
                                            control-id))))
    (setf (context-identity new-obj) (list base-namespace control-id))
    (setf (context-identity-uuid new-obj) new-nmspc)
    new-obj))


;; (find-method #'context-object-uuid-description nil '(context-object-uuid))
(defmethod context-object-uuid-description ((context-object context-object-uuid) &key stream verbose)
  (declare (type boolean verbose))
  (if (not verbose)
      (let* ((unbound "#<UNBOUND>")
             (context-id-if  (context-identity context-object))
             (context-id     (if context-id-if
                             (prin1-to-string context-id-if)
                             unbound))
             (context-id-uuid   (or (context-identity-uuid context-object)
                                unbound)))
        (with-standard-io-syntax 
          (format stream "~%~{~4T~29A~A~^~%~}~4T"
                  (list 
                   ;; "TYPE-OF:"    (type-of context-object)
                   ":CONTEXT-IDENTITY"          context-id
                   ":CONTEXT-IDENTITY-UUID"     context-id-uuid))))

      (let* ((unbound "#<UNBOUND>")
             (context-id-if  (context-identity context-object))
             (context-id     (if context-id-if
                             (prin1-to-string context-id-if)
                             unbound))
             (context-id-uuid   (or (context-identity-uuid context-object)
                                unbound))
             (byte-array    (or (context-identity-uuid-byte-array context-object)
                                unbound))
             (bit-vector    (or (context-identity-uuid-bit-vector context-object)
                                unbound))
             (hex-string-if (context-identity-uuid-string-36 context-object))
             (hex-string    (if hex-string-if
                                (prin1-to-string hex-string-if)
                                unbound))
             (integer-128-if (context-identity-uuid-integer context-object))
             (integer-128    (if integer-128-if
                                 (let ((*print-base* 16)
                                       (*print-radix* t)) 
                                   (princ-to-string integer-128-if))
                                 unbound))
             (parent-uuid    (or (context-identity-parent-uuid context-object)
                                 unbound))         
             (version-if    (context-identity-uuid-version context-object))
             (version       (if version-if 
                                (prin1-to-string version-if)
                                unbound))
             (format-description
              (let ((*print-lines* 0))
                (format nil "~%~{~4T~40A~A~%~}"
                        (list ;; "TYPE-OF:"    (type-of context-object)
                         ":CONTEXT-IDENTITY"                  context-id
                         ":CONTEXT-IDENTITY-UUID"             context-id-uuid
                         ":CONTEXT-IDENTITY-PARENT-UUID"      parent-uuid
                         ":CONTEXT-IDENTITY-UUID-STRING-36"   hex-string
                         ":CONTEXT-IDENTITY-UUID-BYTE-ARRAY"  byte-array
                         ":CONTEXT-IDENTITY-UUID-INTEGER"     integer-128
                         ":CONTEXT-IDENTITY-UUID-BIT-VECTOR"  bit-vector
                         ":CONTEXT-IDENTITY-UUID-VERSION"     version)))))
        (with-standard-io-syntax 
          (princ format-description stream)))))

(defmethod describe-object ((context-object context-object-uuid) stream)
  (print (type-of context-object) stream)
  (context-object-uuid-description context-object :stream stream :verbose t))

(defmethod print-object ((context-object context-object-uuid) stream)
  (print-unreadable-object (context-object stream :type t) ;; :identity t)
    (context-object-uuid-description context-object :stream  stream)))
