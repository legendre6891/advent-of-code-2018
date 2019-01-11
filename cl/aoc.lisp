(defpackage :aoc
  (:use :common-lisp)
  (:export :file-read-lines
           :read-input))

(in-package :aoc)

(defun file-read-lines (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line collect line
            finally (close in)))))

(defun read-input (day)
  (file-read-lines (format nil "../inputs/day~a.txt" day)))

(provide :aoc)
