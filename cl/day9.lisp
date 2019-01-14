(load "setup.lisp")

(aoc-package 9)

(defparameter *input*
  (register-groups-bind ((#'parse-integer players marbles))
      ("^(\\d+).*worth (\\d+)" (first (read-input 9)))
    (cons players marbles)))



(defstruct node
  (marble nil)
  (forward nil)
  (backward nil))

(defclass marble-circle ()
  ((index :initform 0)
   nodes))

(defmacro circle-index (circle)
  `(slot-value ,circle 'index))

(defmacro circle-nodes (circle)
  `(slot-value ,circle 'nodes))

(defmacro circle-at (circle n)
  `(aref (circle-nodes ,circle) ,n))

(defmacro circle-forward (circle n)
  `(node-forward (circle-at ,circle ,n)))

(defmacro circle-backward (circle n)
  `(node-backward (circle-at ,circle ,n)))

(defmethod initialize-instance
    :after ((circle marble-circle) &key marble-count)
  (if marble-count
      (progn
        (setf (circle-nodes circle) (make-array (1+ marble-count) :initial-element nil))
        (iter (for i from 0 to marble-count)
              (setf (circle-at circle i) (make-node :marble i)))
        (setf (circle-forward circle 0) 0)
        (setf (circle-backward circle 0) 0))
      (error "Must specify MARBLE-COUNT.")))


(defun insert-between (circle i j marble)
  (setf (circle-at circle marble)
        (make-node :marble marble
                   :forward j
                   :backward i))
  (setf (circle-forward circle i) marble)
  (setf (circle-backward circle j) marble))

(defun circle-reposition (circle n)
  (setf (circle-index circle) n))

(defun insert-node (circle marble)
  (let* ((index (circle-index circle))
         (i (circle-forward circle index))
         (j (circle-forward circle i)))
    (insert-between circle i j marble)
    (circle-reposition circle marble)))

(defun remove-node (circle n)
  (let ((forward (circle-forward circle n))
        (backward (circle-backward circle n)))
    (setf (circle-forward circle backward) forward)
    (setf (circle-backward circle forward) backward)
    (setf (circle-at circle n) (make-node :marble n))
    (circle-reposition circle forward)))

(defun seven-before (circle)
  (let ((index (circle-index circle)))
    (iter (for i from 1 to 7)
          (setf index (circle-backward circle index))
          (finally (return index)))))

(defun divisible-23 (n) (zerop (rem n 23)))



(defun simulate-game (marble-count player-count)
  (let ((circle (make-instance 'marble-circle :marble-count marble-count))
        (scores (make-array player-count :initial-element 0)))
    (iter (for n from 1 to marble-count)
          (for p from 0)
          (if (divisible-23 n)
              (progn
                (let ((seven (seven-before circle)))
                  (remove-node circle (seven-before circle))
                  (incf (aref scores (rem p player-count)) (+ n seven))))
              (insert-node circle n)))
    (list :index (circle-index circle) :nodes (circle-nodes circle)
          :scores scores)))




(defun part-one (input)
  (let ((simulation (simulate-game (cdr input) (car input))))
    (let ((scores (getf simulation :scores)))
      (iter (for s in-vector scores)
            (finding s maximizing s)))))



(format t "Day 9, Part 1: ~A~%" (part-one *input*))
(format t "Day 9, Part 2: ~A~%" (part-one (cons (car *input*) (* 100 (cdr *input*)))))

