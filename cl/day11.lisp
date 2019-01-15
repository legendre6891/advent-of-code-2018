(load "setup.lisp")
(aoc-package 11)

(defparameter *input* 3214)

(defun third-digit (n)
  (rem (floor (/ n 100)) 10))

(defun power-level (serial x y)
  (let ((rack-id (+ 10 x)))
      (-> rack-id
          (* y)
          (+ serial)
          (* rack-id)
          (third-digit)
          (- 5))))


(defun make-fuel-grid (serial)
  (let ((array (make-array '(300 300) :element-type '(signed-byte 32))))
  (iter (for x from 1 to 300)
        (iter (for y from 1 to 300)
              (setf (aref array (1- x) (1- y))
                    (power-level serial x y))))
    array))


(defun max-row-sum (grid n row)
  (let ((array (make-array 300)))
    (iter (for y from 0 below 300)
          (setf (aref array y)
                (iter (for i from 0 below n)
                      (sum (aref grid (+ row i) y)))))

    (iter (for y from 0 to (- 300 n))
          (let ((sum
                  (iter (for i from 0 below n)
                        (sum (aref array (+ y i))))))

            (finding (list :sum sum :y y) maximizing sum)))))

(defun largest-subgrid-location (grid n)
  (iter (for x from 0 to (- 300 n))
        (let* ((result (max-row-sum grid n x))
               (y (getf result :y))
               (sum (getf result :sum)))
          (finding (list :location (list (1+ x) (1+ y)) :sum sum)
                   maximizing sum))))

(format t "Day 11, Part 1: ~A~%" (largest-subgrid-location (make-fuel-grid *input*) 3))


(defun largest-subgrid (grid)
  (iter (for s from 1 to 300)
        (let ((location (largest-subgrid-location grid s)))
          (format t "   Size: ~A at ~A~%" s location)
          (finding (list :size s :location location) maximizing (getf location :sum)))))


(format t "Day 11, Part 2: ~A~%" (largest-subgrid (make-fuel-grid *input*)))
