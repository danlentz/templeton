;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;
;;;; uptime.lisp
;;;;    
;;;;

(in-package :templeton)

(export 'uptime)

(defvar *start-time* (get-universal-time))

(let ((start-time (get-universal-time)))
  (defun uptime (&optional since suppress-output)
    "Displays start time and current uptime for this lisp process. Or not.
Returns uptime in seconds."
    (let ((start-time (or since start-time))) 
      (let ((uptime (- (get-universal-time) start-time))
             (days '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
        (unless suppress-output
          ;; Pretty print start time
          (multiple-value-bind
            (second minute hour date month year day daylight-p zone)
	    (decode-universal-time start-time)
            (flet ((tz-string (zone daylight-p)
                     (multiple-value-bind (hours mins)
		       (truncate (* zone 60) 60)
                       (when daylight-p
                         (setf hours (1- hours))) ; 1+ in the ISO world.
                       (let ((sign (if (minusp zone) ;ISO tz == -CL tz.
				     #\+
				     #\-)))
                         (format nil "~c~2,'0d~2,'0d" sign (abs hours) (abs mins))))))
              (format t "~&Current image started: ~a, ~
~d-~2,'0d-~2,'0d ~
~2,'0d:~2,'0d:~2,'0d~
~a~%"		    (nth day days) year month date hour minute second
                (tz-string zone daylight-p))))
          ;; Pretty print uptime
          (multiple-value-bind (days day-part)
	    (truncate uptime 86400)
            (multiple-value-bind (hours min-secs)
	      (truncate day-part 3600)
              (multiple-value-bind (minutes seconds)
                (truncate min-secs 60)
                (format t "Uptime: ~[~:;~:*~D days ~]~
                         ~[~:;~:*~D hours ~]~
                         ~[~:;~:*~D minutes ~]~
                         ~[~:;~:*~D seconds~]~%"
                  days hours minutes seconds)
                ;;(format t "Uptime: ~d days ~2,'0d:~2,'0d:~2,'0d~%" days hours minutesseconds)
                ))))
        uptime))))

(uptime)
