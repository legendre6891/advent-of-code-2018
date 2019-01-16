(load "setup.lisp")
(aoc-package 13)


(defstruct cart position direction (cross :left))

;;;;;;;;;;;;;;;;;;
;; Parse input  ;;
;;;;;;;;;;;;;;;;;;

(defparameter *track-translation*
  (map (#\+ :x) (#\- :h) (#\| :v) (#\/ :a)
       (#\\ :b) (#\< :h) (#\> :h) (#\^ :v) (#\v :v)))

(defun make-tracks (input)
  (let ((y-size (size input))
        (x-size (size (@ input 0)))
        (map (empty-map)))
    (dotimes (i x-size)
      (dotimes (j y-size)
        (let* ((char (@ (@ input j) i))
               (track (@ *track-translation* char)))
          (when track
            (includef map (seq i j) track)))))
    map))

(defun make-carts (input)
  (labels ((mc (i j d) (make-cart :position (seq i j) :direction d)))
    (let ((y-size (size input))
          (x-size (size (@ input 0)))
          (carts (empty-set)))
      (dotimes (i x-size)
        (dotimes (j y-size)
          (let* ((char (@ (@ input j) i))
                 (cart
                  (cond
                    ((char= char #\<) (mc i j :left))
                    ((char= char #\>) (mc i j :right))
                    ((char= char #\^) (mc i j :up))
                    ((char= char #\v) (mc i j :down)))))
            (when cart (includef carts cart)))))
      carts)))

(defparameter *input* (image (lambda (x) (convert 'seq x)) (convert 'seq (read-input 13))))
(defparameter *tracks* (make-tracks *input*))
(defparameter *carts* (make-carts *input*))

(defmacro with-cart (cart &rest forms)
  (let ((x (gensym)))
    `(let ((,x ,cart))
       (let ((position (cart-position ,x))
             (direction (cart-direction ,x))
             (cross (cart-cross ,x)))
         (declare (ignorable position direction cross))
         ,@forms))))


(defun sort-carts (carts)
  (labels ((carts-cmp (c d)
             (let ((p (cart-position c))
                   (q (cart-position d)))
               (or (< (@ p 0) (@ q 0))
                   (and (eql (@ p 0) (@ q 0))
                        (< (@ p 1) (@ q 1)))))))
    (sort carts #'carts-cmp)))

(defparameter *new-direction-map*
  (map
   ((seq :x :up :straight) :up)
   ((seq :x :down :straight) :down)
   ((seq :x :left :straight) :left)
   ((seq :x :right :straight) :right)

   ((seq :x :up :left) :left)
   ((seq :x :down :left) :right)
   ((seq :x :left :left) :down)
   ((seq :x :right :left) :up)

   ((seq :x :up :right) :right)
   ((seq :x :down :right) :left)
   ((seq :x :left :right) :up)
   ((seq :x :right :right) :down)

   ((seq :a :up) :right)
   ((seq :a :down) :left)
   ((seq :a :left) :down)
   ((seq :a :right) :up)

   ((seq :b :up) :left)
   ((seq :b :down) :right)
   ((seq :b :left) :up)
   ((seq :b :right) :down)))

(defparameter *new-cross-map*
  (map (:left :straight)
       (:straight :right)
       (:right :left)))

(defparameter *direction-map*
  (map (:left  (seq -1  0))
       (:right (seq  1  0))
       (:up    (seq  0 -1))
       (:down  (seq  0  1))))


(defun new-direction (track cart)
  (with-cart cart
    (match track
      (:v direction)
      (:h direction)
      (:a (@ *new-direction-map* (seq :a direction)))
      (:b (@ *new-direction-map* (seq :b direction)))
      (:x (@ *new-direction-map* (seq :x direction cross))))))

(defun new-cross (track cart)
  (with-cart cart
    (match track
      (:x (@ *new-cross-map* cross))
      (_ cross))))

(defun new-position (cart new-direction)
  (with-cart cart
    (let ((x (@ position 0))
          (y (@ position 1))
          (increment (@ *direction-map* new-direction)))
      (seq (+ x (@ increment 0))
           (+ y (@ increment 1))))))

(defun track-underneath (tracks cart)
  (with-cart cart
    (@ tracks position)))

(defun move-cart (tracks cart)
  (let* ((track (track-underneath tracks cart))
         (new-direction (new-direction track cart))
         (new-position (new-position cart new-direction))
         (new-cross (new-cross track cart)))
    (make-cart :position new-position
               :direction new-direction
               :cross new-cross)))

(defun in-carts (carts cart)
  (let ((position (cart-position cart)))
    (find-if
     (lambda (c) (equal? (cart-position c) position))
     carts)))

(defun removed-cart-from (carts cart)
  (let ((position (cart-position cart)))
    (filter
     (lambda (c) (not (equal? (cart-position c) position)))
     carts)))

(defun evolve-carts (tracks carts)
  ;; Evolves carts, signalling any crashes
  ;; (given as the secondary return value, in the order of crashes)
  ;; and returns a set of remaining carts.
  (let ((carts-today (sort-carts carts))
        (carts-tomorrow (empty-set))
        (crashes (empty-seq)))
    (iter (while (not (zerop (size carts-today))))
       (let* ((cart (first carts-today))
              (new-cart (move-cart tracks cart)))
         ;; remove the cart from today
         (setf carts-today (less-first carts-today))
         (if (or (in-carts carts-today new-cart) (in-carts carts-tomorrow new-cart))
             ;; crash ==> set crashes and remove carts
             (progn
               (setf crashes (with-last crashes new-cart))
               (setf carts-tomorrow (removed-cart-from carts-tomorrow new-cart))
               (setf carts-today (removed-cart-from carts-today new-cart)))
             ;; no crash
             (setf carts-tomorrow (with carts-tomorrow new-cart)))))
    (values carts-tomorrow crashes)))

(defun cart-crash-p (carts)
  (let ((n (size carts))
        (positions (image #'cart-position carts)))
    (not (eql n (size (convert 'set positions))))))

(defun evolve-until-crash (tracks carts)
  (iter (for s from 1)
        (multiple-value-bind (remaining-carts crashes)
            (evolve-carts tracks carts)
          (setf carts remaining-carts)
          (when (not (zerop (size crashes))) (leave crashes)))))

(defun evolve-until-one-remaining (tracks carts)
  (iter (for s from 1)
        (multiple-value-bind (remaining-carts crashes)
            (evolve-carts tracks carts)
          (declare (ignore crashes))
          (setf carts remaining-carts)
          (when (eql 1 (size carts)) (leave carts)))))

(defun location-first-crash (tracks carts)
  (let ((crashes (evolve-until-crash tracks carts)))
    (cart-position (first crashes))))

(defun location-last-remaining (tracks carts)
  (cart-position (arb (evolve-until-one-remaining tracks carts))))

(defun format-location (location)
  (format nil "~A,~A" (@ location 0) (@ location 1)))


(format t "Day 13, Part 1: ~A~%" (format-location (location-first-crash *tracks* *carts*)))
(format t "Day 13, Part 2: ~A~%" (format-location (location-last-remaining *tracks* *carts*)))

