;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;
;;;;; some porcelain around break. mostly originates from dwim.hu 

(in-package :templeton)

(def (macro e) break* ((&key io-format debugger-format inspect) &body forms)
  `(restart-case
     (bind:bind ((values                     (multiple-value-list (progn ,@forms)))
                  (single-value              (if (eql (length values) 1) (first values) values))
                  (swank::*buffer-package*   *package*)
                  (swank::*buffer-readtable* *readtable*))     
       (declare (ignorable single-value))       
       ,@(when io-format `((format *debug-io* ,io-format values)))
       ,@(when inspect   `((swank::inspect-in-emacs single-value :wait nil)))
       ,@(when debugger-format `((break ,debugger-format values)))
       (swank::quit-inspector)
       (values-list values))
     
     (return-values-from-break ()
       :report (lambda (stream)
                 (format stream "Continue by returning different values from BREAK."))
       (format *debug-io* "Enter new return values: ")
       (eval (read)))))

(setf (macro-function :break*) (macro-function 'break*))

(def (macro e) break/print* ((&rest args &key &allow-other-keys) &body forms)
  `(break* 
     (,@args :io-format "Stopping in debugger with multiple values: ~{~A ~}~%"
       :debugger-format "~{~A ~}") ,@forms))

(setf (macro-function :break/print*) (macro-function 'break/print*))

(def (macro e) break/print (&body forms)
  `(break/print* ()
     ,@forms))

(setf (macro-function :break/print) (macro-function 'break/print))


(def (macro e) break/inspect* ((&rest args &key &allow-other-keys) &body forms)
  `(break/print* (,@args :inspect t)
     ,@forms))


(def (macro e) break/inspect (&body forms)
  `(break/inspect* ()
     ,@forms))

(setf (macro-function :break/inspect) (macro-function 'break/inspect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; examplars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; break/print:
;;   support display and optionally modify
;;   variables and directly return values in their specific
;;   lexical context. ex. (foo 2 3) (:foo 2 3)

(defun foo (x y)
  (* x (break/print :y y)))

(defun :foo (x y)
  (* x (:break/print :y y)))


;; break/inspect:
;;   additionally
;; (bar 2 3) (:bar 2 3)

(defun bar (x y)
  (* x (break/inspect :y y)))

(defun :bar (x y)
  (* x (:break/inspect :y y :z 9)))





#|
(defun :break (name &rest values)
  (break "~S = ~{~S~^, ~}" name values)
  (values-list values))

;; ex:
(defun bar (x y)
  (* x (:break :y y)))

(bar 2 3)
|#



