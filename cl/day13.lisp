(load "setup.lisp")

(aoc-package 13)

(defparameter *input* (read-input 13))

(defstruct cart position direction cross)

(defun track-at (tracks position)
  (let ())
  (error "Unimplemented"))


(defun move-cart (tracks cart)
  (let* ((position (cart-position cart))
         (direction (cart-direction cart))
         (cross (cart-cross cart))
         (track (track-at tracks position)))
    (let ((increment (increment track direction cross))
          (new-direction (direction track direction))
          (new-cross (cross track direction cross)))
      (make-cart :position (position-increment position increment)
                 :direction new-direction
                 :cross new-cross))))


(let ((x (make-cart :position '(3 4)
                    :direction :clockwise
                    :cross :left)))
  (trivia:match x
    ((cart :position position :direction direction :cross cross)
     (list position direction cross))))


(let ((x (make-cart :position '(3 4)
                    :direction :clockwise
                    :cross :left)))
  x)

;;;; Build the 150 x 150 array
