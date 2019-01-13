(load "aoc.lisp")

(defpackage :aoc2018.day5
  (:use :common-lisp :aoc :iter
        :alexandria))

(in-package :aoc2018.day5)


(defparameter *input* (first (read-input 5)))

(defun inversep (x y)
  (and (char-equal x y) (not (char= x y))))

(defun string-reductions (string)
  (labels ((reduction-helper (init string remove-list)
             (iter (for x in-string string)
                   (for previous-char previous x initially init)
                   (for i from 1)
                   
                   (when (eql (first remove-list) (1- i))
                     (next-iteration))
                   (when (inversep x previous-char)
                     (push (1- i) remove-list)
                     (push i remove-list))
                   (finally (return (nreverse remove-list))))))
    (reduction-helper (elt string 0) (subseq string 1) '())))

(defun string-except-indices (string indices)
  (format nil "~{~A~}"
          (iter (for x in-string string)
                (for i from 0)
                (if (eql i (first indices))
                    (pop indices)
                    (collect x)))))

(defun reduce-string (string)
  (if-let (reductions (string-reductions string))
    (reduce-string (string-except-indices string reductions))
    string))



(defparameter *reduced-input* (reduce-string *input*))


;;;;; Part 1
(format t "Day 5, Part 1: ~A~%" (length *reduced-input*))



;;;;; Part 2

(defun unique-chars (string)
  (remove-duplicates string :test #'char-equal))

(defun reduce-string-length (string)
  (length (reduce-string string)))

(defun string-without-char (string char)
  (remove-if (lambda (x) (char-equal x char)) string))


(defun shortest-oneshot-reduction (string)
  (let ((unique-chars (unique-chars string)))
    (iter (for c in-string unique-chars)
          (minimize (reduce-string-length (string-without-char string c))))))


(format t "Day 5, Part 2: ~A~%" (shortest-oneshot-reduction *reduced-input*))

