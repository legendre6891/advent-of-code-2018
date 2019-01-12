(load "aoc.lisp")
(ql:quickload :cl-ppcre)

(defpackage :aoc2018.day4
  (:use :common-lisp :aoc
        :cl-ppcre))

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


(defun compare-time (x y)
  ;;; return true if s < t
  (labels ((tiebreak-compare (x y)
             (if (not x) nil
                 (let ((a (first x))
                       (b (first y)))
                   (cond ((< a b) t)
                         ((> a b) nil)
                         (t (tiebreak-compare (rest x) (rest y))))))))
    (tiebreak-compare x y)))


(defparameter *records*
  (sort (mapcar #'parse-record *input*)
        #'compare-time :key #'get-time))


