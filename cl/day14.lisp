(load "setup.lisp")
(aoc-package 14)

(defparameter *input* 320851)

(defun make-initial-state ()
  (map (:recipes (make-array 2
                             :element-type '(unsigned-byte 4)
                             :fill-pointer 2
                             :adjustable t :initial-contents (list 3 7)))
       (:a 0)
       (:b 1)))

(defun integer-to-digits (n)
  (recur digits ((digits (list))
                       (n n))
    (if (< n 10)
        (cons n digits)
        (multiple-value-bind (q r)
            (floor n 10)
          (digits (cons r digits) q)))))

(defparameter *new-recipes-rules*
  (map (0 '(0)) (1 '(1)) (2 '(2)) (3 '(3)) (4 '(4)) (5 '(5))
       (6 '(6)) (7 '(7)) (8 '(8)) (9 '(9)) (10 '(1 0)) (11 '(1 1))
       (12 '(1 2)) (13 '(1 3)) (14 '(1 4)) (15 '(1 5)) (16 '(1 6))
       (17 '(1 7)) (18 '(1 8))))

(defun evolve-recipes (state)
  (declare (optimize (speed 3)))

  (let ((recipes (@ state :recipes))
        (a (@ state :a))
        (b (@ state :b)))
    (let* ((x (@ recipes a))
           (y (@ recipes b))           
           (additional-recipes (@ *new-recipes-rules* (+ x y))))
      (iter (for x in additional-recipes)
            (vector-push-extend x (@ state :recipes)))
      (setf (@ state :a) (mod (+ 1 x a) (length (@ state :recipes))))
      (setf (@ state :b) (mod (+ 1 y b) (length (@ state :recipes))))
      state)))

(defun recipe-at-generation (state n)
  (if (zerop n) (@ state :recipes)
      (recipe-at-generation (evolve-recipes state) (1- n))))

(defun is-subsequence-at (seq sub i)
  (let ((n (length sub)))
    (iter (for x from 0 below n)
          (if (eql (aref seq (+ x i)) (first sub))
              (pop sub)
              (leave nil))
          (finally (return t)))))

(defun find-subsequence (seq s)
  (let ((x (length seq))
        (y (length s)))
    (iter (for i from 0 to (- x y))
          (finding i such-that (is-subsequence-at seq s i)))))

(defparameter *recipe-list*
  (recipe-at-generation (make-initial-state) (* 50 *input*)))

(format t "Day 14, Part 1: ~A~%"
        (subseq *recipe-list* *input* (+ 10 *input*)))

(format t "Day 14, Part 2: ~A~%" (find-subsequence *recipe-list* (list 3 2 0 8 5 1)))


