(defparameter *input*
  (let ((in (open "day3.txt" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line collect line
            finally (close in)))))

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
    (list (to-int (+ 2 at) comma)
          (to-int (+ 1 comma) colon)
          (to-int (+ 2 colon) cross)
          (to-int (+ 1 cross) nil)))))

(defun increment-grid (x-offset y-offset across down)
  (loop for i from y-offset below (+ y-offset down)
        do
        (loop for j from x-offset below (+ x-offset across)
              do (incf (aref *grid* i j)))))


;;; Part 1
(let ((patches (map 'list #'parse-description *input*)))
  (dolist (patch patches)
    (apply #'increment-grid patch)))


(loop for i from 0 below 1000 sum
      (loop for j from 0 below 1000
            counting (> (aref *grid* i j) 1)))




(loop for i from 0 below 1000
      append
      (loop for j from 0 below 1000
            if (eql (aref *grid* i j))
            collect j))
