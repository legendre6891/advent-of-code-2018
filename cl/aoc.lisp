(load "~/.sbclrc")

(ql:quickload :metabang-bind :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :iterate :silent t)
(ql:quickload :group-by :silent t)


(defpackage :aoc
  (:use :common-lisp :metabang-bind)
  (:export :file-read-lines
           :read-input :tiebreak-compare))

(in-package :aoc)

(defun file-read-lines (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line collect line
            finally (close in)))))

(defun read-input (day)
  (file-read-lines (format nil "../inputs/day~a.txt" day)))



(defun tiebreak-compare (x y)
  (when (not (eql (length x) (length y)))
    (error (format nil "X and Y of unequal lengths.")))
  (labels ((f (x y)
             (if (not x) nil
                 (let ((a (first x))
                       (b (first y)))
                   (cond ((< a b) t)
                         ((> a b) nil)
                         (t (tiebreak-compare (rest x) (rest y))))))))
    (f x y)))

(provide :aoc)
