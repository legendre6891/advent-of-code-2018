(load "aoc.lisp")
(ql:quickload :cl-ppcre)
(ql:quickload :alexandria)
(ql:quickload :iterate)

(defpackage :aoc2018.day4
  (:use :common-lisp :aoc
        :cl-ppcre :alexandria :iterate))

(in-package :aoc2018.day4)

(defparameter *input* (read-input 4))


(defparameter one (first *input*))
(defparameter two (second *input*))
(defparameter three (third *input*))


(defun parse-record (string)
  ;;; better just pull in the regex library
  (let ((timestamp
          (register-groups-bind (month day hour minute)
                 ("\\[\\d{4}-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\]" string)
            (mapcar #'parse-integer (list month day hour minute))))

        (action
          (cond ((scan "wakes" string) :wake-up)
                   ((scan "falls" string) :sleeps)
                   (t
                    (register-groups-bind ((#'parse-integer guard))
                        ("#(\\d+)" string) guard)))))
    (if (numberp action)
          (list :time timestamp :guard action :action :start)
          (list :time timestamp :guard nil :action action))))

(defun get-time (record) (getf record :time))
(defun get-action (record) (getf record :action))
(defun get-guard (record) (getf record :guard))

(defun guardp (record) (get-guard record))

(defun compare-time (x y)
  (tiebreak-compare x y))



(defparameter *records*
  (sort (mapcar #'parse-record *input*)
        #'compare-time :key #'get-time))



;;; ==== Part 1 =====

(defun partition-records (records)
  (let ((groups '())
        (current-group '()))
    (iter (for record in records)
          (if (guardp record)
              (progn
                (push (reverse current-group) groups)
                (setf current-group (list record)))
              (push record current-group)))

    (push (reverse current-group) groups)
    (nreverse groups)))


(defun normalize-start-time (record)
  (let* ((time (get-time record))
         (month)
         (hour ())
         )

    ()
    )



  )



(loop for x in '(1 2 3 4)
      if (evenp x) do (progn (format t "Even number: ") (format t "~a~%" x)))


(iter (for x in '(1 2 3 4))
      (collect x))
