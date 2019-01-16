(load "setup.lisp")
(aoc-package 14)

(defparameter *input* 320851)

(defparameter *initial-state*
  (map (:recipes (seq 3 7))
       (:a 0)
       (:b 1)))


(defun integer-to-digits (n)
  (recur digits ((digits (list))
                       (n n))
    (if (< n 10)
        (convert 'seq (cons n digits))
        (multiple-value-bind (q r)
            (floor n 10)
          (digits (cons r digits) q)))))


(defparameter *new-recipes-rules*
  (let ((map (empty-map)))
    (iter (for i from 0 to 9)
          (iter (for j from i to 9)
                (adjoinf
                 map (bag i j)
                 (integer-to-digits (+ i j)))))
    map))

(defun evolve-recipes (state)
  (let ((recipes (@ state :recipes))
        (a (@ state :a))
        (b (@ state :b)))
    (let* ((x (@ recipes a))
           (y (@ recipes b))           
           (additional-recipes (@ *new-recipes-rules* (bag x y)))
           (new-recipes (concat recipes additional-recipes)))
      (map (:recipes new-recipes)
           (:a (mod (+ 1 x a) (size new-recipes)))
           (:b (mod (+ 1 y b) (size new-recipes)))))))


(defun recipe-at-generation (state n)
  (if (zerop n) (@ state :recipes)
      (recipe-at-generation (evolve-recipes state) (1- n))))


(defparameter *recipe-list*
  (recipe-at-generation *initial-state* (* 100 *input*)))


(defun find-subsequence (seq s)
  (let ((x (size seq))
        (y (size s)))
      (iter (for i from 0 to (- x y))
        (finding i such-that (equal? s (subseq seq i (+ i y)))))))


(format t "Day 14, Part 1: ~A~%"
        (subseq (recipe-at-generation *initial-state* (+ 12 *input*))
                *input* (+ 10 *input*)))


