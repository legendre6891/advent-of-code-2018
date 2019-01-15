(load "setup.lisp")
(aoc-package 10)

(defstruct point
  x y v w)

(defun parse-input (line)
  (register-groups-bind ((#'parse-integer x y v w))
      ("position=<(.*), (.*)> velocity=<(.*), (.*)>" line)
    (make-point :x x :y y :v v :w w)))

(defparameter *input*
  (mapcar #'parse-input
          (read-input 10)))


(defun move-point (point)
  (incf (point-x point) (point-v point))
  (incf (point-y point) (point-w point)))


(defun maximize-x (input)
  (iter (for point in input)
        (finding (point-x point) maximizing (point-x point))))

(defun minimize-x (input)
  (iter (for point in input)
        (finding (point-x point) minimizing (point-x point))))


(defun maximize-y (input)
  (iter (for point in input)
        (finding (point-y point) maximizing (point-y point))))

(defun minimize-y (input)
  (iter (for point in input)
        (finding (point-y point) minimizing (point-y point))))


(defun max-x-difference (input)
  (- (maximize-x input) (minimize-x input)))

(defun max-y-difference (input)
  (- (maximize-y input) (minimize-y input)))




(defun move-stars (input)
  (iter (for point in input)
        (move-point point)))

(defun create-stars ()
  (mapcar #'parse-input
          (read-input 10)))


(defun star-at-p (input x y)
  (iter (for point in input)
        (finding point such-that
                 (and (eql (point-x point) x)
                      (eql (point-y point) y)))))



(defun normalize-input (input)
  (let ((x (minimize-x input))
          (y (minimize-y input)))
      (iter (for point in input)
            (decf (point-x point) x)
            (decf (point-y point) y))))

(defun draw-normalized-input (input)
  (iter (for y from 0 to (maximize-y input))
      (iter (for x from 0 to (maximize-x input))
            (if (star-at-p input x y)
                (format t "#")
                (format t "."))
            (finally (format t "~%")))))





(defparameter *collapsed*
  (let ((input (create-stars)))
    (iter (repeat 50000)
          (for i from 0)
          (iter (for point in input) (move-point point))
          (when (eql (max-x-difference input) 61)
            (format t "After ~A seconds, we have:~%~%" (1+ i))
            (finish)))
    (normalize-input input)
    (draw-normalized-input input)
    (format t "~%~%")
    input))
