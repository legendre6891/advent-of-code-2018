(load "setup.lisp")
(aoc-package 3)


(defparameter *input* (read-input 3))

(defparameter *grid*
  (make-array '(1000 1000)
              :element-type 'fixnum
              :initial-element 0))

(defun parse-description (description)
  (labels ((pos (char)
             (position char description :test #'eql))
           (to-int (x y)
             (parse-integer description :start x :end y)))
  (let* ((at (pos #\@))
         (comma (pos #\,))
         (colon (pos #\:))
         (cross (pos #\x)))
    (list (to-int 1 (- at 1))
          (to-int (+ 2 at) comma)
          (to-int (+ 1 comma) colon)
          (to-int (+ 2 colon) cross)
          (to-int (+ 1 cross) nil)))))

(defun get-id (patch) (first patch))

(defvar *patches* (map 'list #'parse-description *input*))



;;; ============ Part 1 =============

(defun increment-grid (grid x-offset y-offset across down)
  (loop for i from y-offset below (+ y-offset down) do
        (loop for j from x-offset below (+ x-offset across)
              do (incf (aref grid i j)))))


(defun part-one (patches)
 (let ((grid (make-array '(1000 1000) :element-type 'fixnum :initial-element 0)))
   (dolist (patch patches)
     (apply #'increment-grid grid (rest patch)))
   (loop for i from 0 below 1000 sum
         (loop for j from 0 below 1000
               counting (> (aref grid i j) 1)))))


(format t "Day 3, Part 1: ~A~%" (part-one *patches*))



;;; ============ Part 2 =============
(defun intersectp (x y a b)
  (<= (max x a) (min y b)))

(defun patch-to-interval (patch)
  (destructuring-bind (id x y a b) patch
    (declare (ignore id))
    (list (list x (1- (+ x a)))
          (list y (1- (+ y b))))))

(defun overlap-p (patch-one patch-two)
  (destructuring-bind ((A B) (C D)) (patch-to-interval patch-one)
    (destructuring-bind ((X Y) (W Z)) (patch-to-interval patch-two)
      (and (intersectp A B X Y) (intersectp C D W Z)))))


(defun non-overlap-among (patches x)
  ;;; true if x doesn't overlap among any of the patch in patches
  ;;; except possibly with another patch with the same ID
  (let ((id (get-id x)))
    (loop for patch in patches
          never (and (overlap-p x patch)
                     (not (eql id (get-id patch)))))))

(defun part-two (patches)
  (get-id
   (loop for patch in patches
         if (non-overlap-among patches patch) return patch)))

(format t "Day 3, Part 2: ~A~%" (part-two *patches*))


