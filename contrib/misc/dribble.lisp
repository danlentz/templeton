;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :templeton)

(def (function e) start-dribble (&optional filename)
  "We dribble to a timestamped file in a specific #P\"HOME:DRIBBLE;\" directory."
  (let ((path (or filename
                (merge-pathnames
                  (make-pathname
                    :directory '(:relative ".cache" "dribble")
                    :name (flet ((implementation-id ()
                                   (flet ((first-word (text)
                                            (let ((pos (position (character " ") text)))
                                              (remove (character ".")
                                                (if pos (subseq text 0 pos) text)))))
                                     (format
                                       nil
                                       "~A-~A-~A"
                                       (cond 
                                         ((string-equal
                                            "International Allegro CL Enterprise Edition"
                                            (lisp-implementation-type))
                                           "ACL")
                                         (t (first-word (lisp-implementation-type))))
                                       (first-word (lisp-implementation-version))
                                       (first-word (machine-type))))))
                            (multiple-value-bind (se mi ho da mo ye)
                              (decode-universal-time (get-universal-time))
                              (format nil "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D-~A"
                                ye mo da ho mi se (implementation-id))))
                    :type "log"
                    :version nil
                    :defaults (user-homedir-pathname))
                  (user-homedir-pathname) nil))))
    (ensure-directories-exist path)
    (dribble path :if-exists :append)))

(def (function e) stop-dribble ()
  (dribble)) 


