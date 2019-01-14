(load "setup.lisp")
(aoc-package 8)

(defparameter *input*
  (->> "../inputs/day8.txt"
      file-read-lines
      first
      (split-sequence #\space)
      (mapcar #'parse-integer)))

(defstruct node
  (children nil)
  (metadata nil)
  (sum 0))


(defun parse-tree (list)
  (labels ((parse-leaves (n list tree)
             (if (zerop n)
                 (values (reverse tree) list)
                 (let ((n-children (first list))
                        (n-metadata (second list)))
                   (multiple-value-bind (children remainder)
                       (parse-leaves n-children (cddr list) '())
                     (multiple-value-bind (metadata rest) (split-at n-metadata remainder)
                       (parse-leaves (1- n) rest
                                     (cons (make-node
                                            :children children
                                            :metadata metadata
                                            :sum (apply #'+ metadata))
                                           tree))))))))
    (first (parse-leaves 1 list '()))))


;;;;; Part 1
(defun reduce-tree (tree node-fn combinator initially)
  (labels ((reducer (node)
             (if (not node)
                 initially
                 (apply combinator (funcall node-fn node)
                        (mapcar #'reducer (node-children node))))))
    (reducer tree)))

(defun tree-metadata-sum (tree) (reduce-tree tree #'node-sum #'+ 0))

(format t "Day 8, Part 1: ~A~%"
        (tree-metadata-sum (parse-tree *input*)))



;;;;; Part 2
(defun node-value (node)
  (if (not node) 0
      (if (node-children node)
          (let ((indexed-children
                  (mapcar (lambda (x) (nth (1- x) (node-children node))) (node-metadata node))))
            (apply #'+ (mapcar #'node-value indexed-children)))
          (node-sum node))))

(format t "Day 8, Part 2: ~A~%"
        (node-value (parse-tree *input*)))

