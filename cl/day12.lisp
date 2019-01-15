(load "setup.lisp")
(aoc-package 12)

(defparameter *input* (read-input 12))

(defun plant-char-p (c)
  (char= c #\#))

(defun bits-index (bits)
  (when (not (eql (length bits) 5))
    (error "Bits longer than 5 encountered"))
  (let ((counter 0))
      (iter (for b in bits)
            (for i from 0)
            (when b (incf counter (expt 2 i))))
    counter))

(defun pattern-index (pattern)
  (bits-index (cl:map 'list #'plant-char-p pattern)))

(defun min-plant (plants)
  (extremum plants #'<))

(defun max-plant (plants)
  (extremum plants #'>))

(defparameter *initial-state*
  (let ((string (subseq (first *input*) 15))
        (set (fset:empty-set)))
    (iter (for s in-vector string)
          (for i from 0)
          (when (plant-char-p s)
            (setf set (fset:with set i)))
          (finally (return set)))))

(defparameter *rules*
  (let ((array (make-array 32 :element-type 'boolean :initial-element nil)))
    (iter (for string in (cddr *input*))
          (setf (aref array (pattern-index (subseq string 0 5)))
                (plant-char-p (elt string (1- (length string))))))
    array))


(defun plant-grows-p (plants rules x)
  (aref rules
        (iter (for i from -2 to 2)
              (for weight in '(1 2 4 8 16))
              (when (fset:contains? plants (+ x i)) (sum weight)))))

(defun evolve-around (plants rules p)
  (fset:filter (lambda (x) (plant-grows-p plants rules x))
               (fset:set p (1+ p) (1- p) (+ 2 p) (- p 2))))

(defun evolve (plants rules)
  (fset:reduce #'fset:union
               (fset:image (lambda (x) (evolve-around plants rules x)) plants)))


(defun generation (plants rules n)
  (if (zerop n) plants
      (generation (evolve plants rules) rules (1- n))))

(defun state-statistics (state)
  (let ((sum (fset:reduce #'+ state))
        (max (fset:reduce #'max state))
        (min (fset:reduce #'min state)))
    (list :sum sum
          :max max
          :min min
          :range (- max min))))


(defun generation-statistics (plants rules n)
  (state-statistics (generation plants rules n)))

(defun normalized-state (state)
  (let ((min (fset:reduce #'min state)))
    (fset:image (lambda (x) (- x min)) state)))


;;;; Run this to find that the range is fixed at 177
;;;; which means that the problem must be periodic
(let ((state *initial-state*))
  (iter (for n from 1 to 1000)
        (setf state (evolve state *rules*))
        (format t "~A: ~A~%" n (state-statistics state))))


;;;; Let t be the first time that the range is 177
;;;; For my input, it turns out to be 183
(defparameter *t*
  (let ((state *initial-state*)
        (magic-number 177))
    (iter (for n from 1)
          (setf state (evolve state *rules*))
          (let ((statistics (state-statistics state)))
            (finding n such-that (eql (getf statistics :range) magic-number))))))

;;;; Let *tau* be the period, so that the state at time *t* + *tau*
;;;; is the same as at time *t*. For us, *tau* = 1 (!!!).
(defparameter *tau*
  (let ((fixed-state (normalized-state (generation *initial-state* *rules* *t*)))
        (state (normalized-state (generation *initial-state* *rules* *t*))))
    (iter (for tau from 1)
          (setf state (normalized-state (evolve state *rules*)))
          (finding tau such-that (fset:equal? state fixed-state)))))


;;;; After that it's easy, since the sum is just a linear function.