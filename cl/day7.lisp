(load "setup.lisp")
(aoc-package 7)


(defstruct node
  (status 'non-existent)
  (level 0)
  (reqs '()))

(defun char-index (c) (- (char-int c) 65))

(defparameter *input*
  (mapcar (lambda (x)
            (mapcar #'char-index
                    (list (aref x 36) (aref x 5))))
          (read-input 7)))


(defun create-graph (inputs)
  (let ((graph (make-array 26)))
    (iter (for i from 0 to 25)
          (setf (aref graph i) (make-node)))

    (iter (for x in (remove-duplicates (flatten inputs)))
          (setf (node-status (aref graph x)) 'pending))

    (iter (for input in inputs)
          (let ((x (first input))
                (pre-req (second input)))
            (incf (node-level (aref graph x)))
            (push pre-req (node-reqs (aref graph x)))))
    graph))


(defun jobs-done (graph)
  (iter (for node in-vector graph)
        (always (or (eq (node-status node) 'non-existent)
                    (eq (node-status node) 'done)))))

(defun node-ready (node)
  (and (eq (node-status node) 'pending)
       (zerop (node-level node))))

(defun first-available-task (graph)
  (iter (for node in-vector graph)
        (for i from 0)
        (finding i such-that (node-ready node))))

(defun finish-task (graph n)
  (setf (node-status (aref graph n)) 'done)
  (iter (for node in-vector graph)
        (when (find n (node-reqs node))
          (decf (node-level node)))
        (finally (return graph))))


;;;;; Part 1

(defun step-one (graph)
  (let (task)
    (iter (while (not (jobs-done graph)))
          (setf task (first-available-task graph))
          (collect (code-char (+ 65 task)) result-type string)
          (finish-task graph task))))

(format t "Day 7, Part 1: ~A~%"
        (step-one (create-graph *input*)))




;;;;;; Part 2

(defstruct worker
  (task nil)
  (timer 0))

(defun worker-available-p (worker)
  (not (worker-task worker)))

(defun tick-worker (worker)
  (when (worker-task worker)
    (decf (worker-timer worker))))

(defun free-worker (worker)
  (setf (worker-task worker) nil)
  (setf (worker-timer worker) 0))

(defun worker-finished-p (worker)
  (and (worker-task worker) (zerop (worker-timer worker))))

(defun make-worker-pool (n)
  (iter (repeat n) (collect (make-worker))))

(defun pool-open (worker-pool)
  (iter (for worker in worker-pool)
        (finding worker such-that (worker-available-p worker))))

(defun pool-finished-p (worker-pool)
  (iter (for worker in worker-pool)
        (thereis (worker-finished-p worker))))

(defun tick (timings worker-pool graph)
  (labels ((assign-worker (worker task)
             (setf (node-status (aref graph task)) 'in-progress)
             (setf (worker-task worker) task)
             (setf (worker-timer worker) (elt timings task))))

    (iter (for worker in worker-pool)
          (tick-worker worker))

    ;; update the task graph and free up the workers
    (iter (for worker in worker-pool)
          (when (worker-finished-p worker)
            (finish-task graph (worker-task worker))
            (free-worker worker)))

    ;; assign free workers to available tasks
    (iter (for node in-vector graph)
          (for task from 0)
          (when (node-ready node)
            (if-let (worker (pool-open worker-pool))
              (assign-worker worker task)
              (finish))))

    (list worker-pool graph)))



(defparameter *test-input*
  (mapcar (lambda (x)
            (mapcar #'char-index
                    (list (aref x 36) (aref x 5))))
          (file-read-lines "./asdf.txt")))

(defparameter *test-timings* '(1 2 3 4 5 6))

(defun simulate (input timings n-workers)
  (let ((worker-pool (make-worker-pool n-workers))
        (graph (create-graph input)))

    (iter (for ticks from 0)
          (while (not (jobs-done graph)))
          (tick timings worker-pool graph)

          ;; One off error because we require the final tick
          ;; to set done states
          (finally (return (1- ticks))))))


(format t "Day 7, Part 2: ~A~%"
        (let ((timings (iota 26 :start 61)))
          (simulate *input* timings 5)))
