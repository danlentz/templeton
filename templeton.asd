;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

;;;
;;;;  templeton.asd
;;;
;;;
;;;   based on:
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   WILBUR2: Nokia Semantic Web Toolkit for CLOS
;;;
;;;   Copyright (c) 2001-2009 Nokia Corp. and/or its subsidiaries. All Rights Reserved.
;;;   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
;;;
;;;   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
;;;
;;;   This program is licensed under the terms of the GNU Lesser General Public License
;;;   as published by the Free Software Foundation, version 2.1 of the License. Note
;;;   however that a preamble attached below also applies to this program.
;;;
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;   Preamble to the Gnu Lesser General Public License
;;;
;;;   Copyright (c) 2000 Franz Incorporated, Berkeley, CA 94704
;;;
;;;   The concept of the GNU Lesser General Public License version 2.1 ("LGPL") has been
;;;   adopted to govern the use and distribution of above-mentioned application. However,
;;;   the LGPL uses terminology that is more appropriate for a program written in C than
;;;   one written in Lisp. Nevertheless, the LGPL can still be applied to a Lisp program
;;;   if certain clarifications are made. This document details those clarifications.
;;;   Accordingly, the license for the open-source Lisp applications consists of this
;;;   document plus the LGPL. Wherever there is a conflict between this document and the
;;;   LGPL, this document takes precedence over the LGPL.
;;;
;;;   A "Library" in Lisp is a collection of Lisp functions, data and foreign modules.
;;;   The form of the Library can be Lisp source code (for processing by an interpreter)
;;;   or object code (usually the result of compilation of source code or built with some
;;;   other mechanisms). Foreign modules are object code in a form that can be linked
;;;   into a Lisp executable. When we speak of functions we do so in the most general way
;;;   to include, in addition, methods and unnamed functions. Lisp "data" is also a
;;;   general term that includes the data structures resulting from defining Lisp classes.
;;;   A Lisp application may include the same set of Lisp objects as does a Library, but
;;;   this does not mean that the application is necessarily a "work based on the Library"
;;;   it contains.
;;;
;;;   The Library consists of everything in the distribution file set before any
;;;   modifications are made to the files. If any of the functions or classes in the
;;;   Library are redefined in other files, then those redefinitions ARE considered a
;;;   work based on the Library. If additional methods are added to generic functions in
;;;   the Library, those additional methods are NOT considered a work based on the
;;;   Library. If Library classes are subclassed, these subclasses are NOT considered a
;;;   work based on the Library. If the Library is modified to explicitly call other
;;;   functions that are neither part of Lisp itself nor an available add-on module to
;;;   Lisp, then the functions called by the modified Library ARE considered a work based
;;;   on the Library. The goal is to ensure that the Library will compile and run without
;;;   getting undefined function errors.
;;;
;;;   It is permitted to add proprietary source code to the Library, but it must be done
;;;   in a way such that the Library will still run without that proprietary code present.
;;;   Section 5 of the LGPL distinguishes between the case of a library being dynamically
;;;   linked at runtime and one being statically linked at build time. Section 5 of the
;;;   LGPL states that the former results in an executable that is a "work that uses the
;;;   Library." Section 5 of the LGPL states that the latter results in one that is a
;;;   "derivative of the Library", which is therefore covered by the LGPL. Since Lisp only
;;;   offers one choice, which is to link the Library into an executable at build time, we
;;;   declare that, for the purpose applying the LGPL to the Library, an executable that
;;;   results from linking a "work that uses the Library" with the Library is considered a
;;;   "work that uses the Library" and is therefore NOT covered by the LGPL.
;;;
;;;   Because of this declaration, section 6 of LGPL is not applicable to the Library.
;;;   However, in connection with each distribution of this executable, you must also
;;;   deliver, in accordance with the terms and conditions of the LGPL, the source code
;;;   of Library (or your derivative thereof) that is incorporated into this executable. 
;;;
;;; --------------------------------------------------------------------------------------
;;;
;;;
;;;   Purpose: System definition(s) for Wilbur2
;;;
;;;   We no longer support either The CMU Defsystem (by Mark Kantrowitz) nor Franz,
;;;   Inc.'s defsystem (as shipped with Allegro Common Lisp). Instead, after a lot of
;;;   "soul-searching" we -- perhaps a little reluctantly -- have decided to go with
;;;   ASDF. It seems to have become the norm. For your convenience, the function
;;;   MAKE-WILBUR has been defined (internal in the CL-USER package).
;;;
;;;   Wilbur relies on the logical pathname host "wilbur". Here's a sample of how to set
;;;   up the pathname translations on a Unix-style system:
;;;
;;;     (("base;**;*.*"  "/Users/ora/Wilbur/**/*.*") ; this line is the example part
;;;      ("nox;*.*"      "wilbur:base;src;nox;*.*")
;;;      ("core;*.*"     "wilbur:base;src;core;*.*")
;;;      ("goodies;*.*"  "wilbur:base;src;goodies;*.*")
;;;      ("libs;**;*.*"  "wilbur:base;src;libs;**;*.*")
;;;      ("doc;*.*"      "wilbur:base;doc;*.*")
;;;      ("schemata;*.*" "wilbur:base;schemata;*.*"))
;;;
;;;   There's code below that attempts to define the above rules. It is not always easy
;;;   to understand how the Common Lisp logical pathname translations work. Please check
;;;   the translations (using TRANSLATE-LOGICAL-PATHNAME) before assuming that there are
;;;   bugs in Wilbur. :-)
;;;


(in-package "ASDF")


;;; --------------------------------------------------------------------------------------
;;;
;;;   COMPATIBILITY STUFF
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; OK, this is a hack, but here goes anyway...
  (when (find-package "UFFI")
    (pushnew :uffi *features*))
  ;;#+:sbcl
  ;;(pushnew :piglet *features*)
  )


;;; --------------------------------------------------------------------------------------
;;;
;;;   LOGICAL PATHNAME STUFF
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (let* ((p (probe-file *load-pathname*))
	 (d (pathname-directory p)))
    (setf (logical-pathname-translations "wilbur")
	  `(("base;**;*.*"
	     ,(merge-pathnames (make-pathname :name :wild :type :wild :version :wild
					      :directory '(:relative :wild-inferiors))
			       (make-pathname :name nil :type nil
					      :directory d ;; (subseq d 0 (1- (length d)))
					      :defaults p)))
	    ("nox;*.*"      "wilbur:base;src;nox;*.*")
	    ("core;*.*"     "wilbur:base;src;core;*.*")
	    ("goodies;*.*"  "wilbur:base;src;goodies;*.*")
	    ("libs;**;*.*"  "wilbur:base;src;libs;**;*.*")
	    ("doc;*.*"      "wilbur:base;doc;*.*")
	    ("schemata;*.*" "wilbur:base;schemata;*.*")
            ("**;*.*"       "wilbur:base;**;*.*")))
    (format t "~&; \"wilbur:base;\" = ~A~%" (translate-logical-pathname "wilbur:base;"))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   ASDF SYSTEM DEFINITION FOR WILBUR2
;;;

(defsystem :templeton
  :name "templeton"
  :author "Ora Lassila mailto:ora.lassila@nokia.com"
  :author "Dan Lentz mailto:danlentz@gmail.com"
  :version "3"
  :licence "LLGPL21"
  :description "Efficient, durable 'graph prevalence' for WILBUR2 and ELEPHANT"
  :depends-on (#+mcl :de.setf.utility.bsd
                #+sbcl :usocket
                :alexandria
                :elephant
                :named-readtables
                :local-time
                :yacc
                :unicly
                :swank
                :cl-ppcre
                :hu.dwim.def
                :hu.dwim.defclass-star
                :hu.dwim.stefil)
  :serial t
  :components
  ((:module :src
     :components ((:file "packages")
                   (:file "platform" :depends-on ("packages"))
                   (:file "useful" :depends-on ("packages"))
                   (:module :nox
                     :components ((:file "core-constants")
                                   (:file "xml-util" :depends-on ("core-constants"))
                                   (:file "xml-parser" :depends-on ("xml-util")))
                     :depends-on ("packages" "platform" "useful"))
                   (:module :core
                     :components ((:file "hash-table")
                                   (:file "data" :depends-on ("hash-table"))
                                   (:file "literal" :depends-on ("data"))
                                   (:file "rdf-parser" :depends-on ("data" "literal"))
                                   (:file "http")
                                   (:file "data-sources" :depends-on ("data" "http"))
                                   (:file "wilbur-ql" :depends-on ("data" "literal"))
                                   (:file "reasoner" :depends-on ("wilbur-ql")))
                     :depends-on (:nox))
                   (:module :goodies
                     :components ((:file "serializer")
                                   #+:realmcl (:file "rdf-inspector" :depends-on ("serializer"))
                                   (:file "ivanhoe")
                                   (:file "db-additions")
                                   (:file "index-and-match"))
                     :depends-on (:nox :core))))
    (:module :contrib
      :serial t
      :components ((:file "package")
                    (:file "parameters")
                    (:file "suite")

                    (:module :misc  :serial t
                      :components ((:file "uptime")
                                    (:file "printv")
                                    (:file "break")
                                    (:file "dribble")
                                    (:file "pp")
                                    (:file "pp-histogram")
                                    (:file "pp-barchart")
                                    (:file "pp-columnar")
                                    (:file "pp-string")
                                    (:file "wrap-string")
                                    (:file "clos")))
                    
                    (:module :index :serial t
                      :components ((:file "scanners")
                                    (:file "collectors")
                                    (:file "mappers")
                                    (:file "lset")
                                    (:file "tries")))

                    (:module :db    :serial t
                      :components ((:file "db-protocol")
                                    (:file "db-statistics")
                              #+()      (:file "hashset-db")))

                    (:module :owl
                      :serial t
                      :components ((:file "owl-parser")))

                    (:module :turtle
                      :serial t
                      :components ((:file "package")
                                    (:file "utilities")
                                    (:file "lexer")
                                    (:file "ast")
                                    (:file "target")
                                    (:file "grammar")
                                    (:file "turtle")
                                    (:file "test")))

                    (:module :elephant
                      :serial t
                      :components ((:file "btree-utilities")
                                    (:file "phash")
                                    (:file "storable")
                                    (:file "snapshot")
                                    (:file "node-db")
                                    (:file "node-db-api")
                                    (:file "frame-api" )))

                    (:module :prefix
                      :serial t
                      :components ((:file "prefix")))))))


;;; --------------------------------------------------------------------------------------
;;;
;;;   HOPEFULLY USEFUL STUFF
;;;

(defun cl-user::build-system (system &key (asd nil) (compilep nil))
  ;; OK, notice we have a HANDLER-BIND here... I give up, I cannot figure out
  ;; how the constant definition process works on SBCL. Perhaps that's the way
  ;; ANSI CL is supposed to work, but it seems difficult to get it right. I
  ;; find it easier to just suppress the "constant-redefined-with-new-value"
  ;; errors :-)
  (handler-bind (#+:sbcl (sb-ext:defconstant-uneql #'continue))
    (when asd
      (load asd))
    (asdf:operate (if compilep 'asdf:compile-op 'asdf:load-op) system)))

(defun cl-user::make-templeton (&key (compilep nil))
  (cl-user::build-system :templeton :compilep compilep))

;;; comment out if you want triples to be structs instead of class instances
(pushnew :wilbur-triples-as-classes *features*)

#+:allegro
(pushnew :wilbur-own-hashtables *features*)
