(load "setup.lisp")
(aoc-package 7)

(defun char-index (c) (- (char-int c) 65))

(defparameter *input*
  (mapcar (lambda (x)
            (mapcar #'char-index
                    (list (aref x 5) (aref x 36))))
          (read-input 7)))



(defun create-associations (inputs)
  (let ((associations (make-array 26)))
    (iter (for i from 0 to 25)
          (setf (aref associations i) (list 0 (list))))    
    (iter (for input in inputs)
          (let ((x (first input))
                (pre-req (second input)))
            (let ((i (char-index x)))
              (incf (first (aref associations i)))
              (push pre-req (second (aref associations i))))))
    associations))

(defun first-available-task (associations)
  (iter (for assoc in-vector associations)
        (for i from 0)
        (finding i such-that (zerop (first assoc)))))

(defparameter *x* (create-associations *input*))

(first-available-task (create-associations *input*))


(map 'list #'first (create-associations *input*))


(let (associations (create-associations *input*))
  


  )


(let ((x (list 0 (list))))
  (push 'A (second x))
  (push 'B (second x))
  x)

(let ((x (make-array 2 :initial-element (list 0))))
  (incf (first (aref x 0)))
  (incf (first (aref x 1)))
  x)


(defun foo (x)
  (setf (elt x 0) 'a))

(let ((x (list 1 2 3)))
  (foo x)
  x)
