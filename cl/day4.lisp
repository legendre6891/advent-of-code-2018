(load "setup.lisp")

(aoc-package 4)

(defparameter *input* (read-input 4))


(defparameter one (first *input*))
(defparameter two (second *input*))
(defparameter three (third *input*))


(defun parse-record (string)
  ;;; better just pull in the regex library
  (let ((timestamp
          (register-groups-bind (month day hour minute)
                 ("\\[\\d{4}-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\]" string)
            (mapcar #'parse-integer (list month day hour minute))))

        (action
          (cond ((scan "wakes" string) :wake-up)
                   ((scan "falls" string) :sleeps)
                   (t
                    (register-groups-bind ((#'parse-integer guard))
                        ("#(\\d+)" string) guard)))))
    (if (numberp action)
          (list :time timestamp :guard action :action :start)
          (list :time timestamp :guard nil :action action))))

(defun get-time (record) (getf record :time))
(defun get-action (record) (getf record :action))
(defun get-guard (record) (getf record :guard))

(defun guardp (record) (get-guard record))


(defun tiebreak-compare (x y)
  (when (not (eql (length x) (length y)))
    (error (format nil "X and Y of unequal lengths.")))
  (labels ((f (x y)
             (if (not x) nil
                 (let ((a (first x))
                       (b (first y)))
                   (cond ((< a b) t)
                         ((> a b) nil)
                         (t (tiebreak-compare (rest x) (rest y))))))))
    (f x y)))

(defun compare-time (x y)
  (tiebreak-compare x y))



(defparameter *records*
  (sort (mapcar #'parse-record *input*)
        #'compare-time :key #'get-time))



;;; ==== Part 1 =====

(defun partition-records (records)
  (let ((groups '())
        (current-group '()))
    (iter (for record in records)
          (if (guardp record)
              (progn
                (when current-group
                  (push (reverse current-group) groups))
                (setf current-group (list record)))
              (push record current-group)))

    (push (reverse current-group) groups)
    (nreverse groups)))

(defparameter *partitions* (partition-records *records*))

(defun minutes-working (shifts)
  (let ((timesheet (make-array 60 :element-type 'bit ))
        (state 0))
    (iter (for x from 0 below 60)
          ;; figure out state
          (iter (while (eql (first shifts) x))
                (pop shifts)
                (setf state (- 1 state)))

          (setf (aref timesheet x) state))
    timesheet))

(defun sum-bit-vector (bit-vector)
  (iter (for x in-vector bit-vector) (sum x)))

(defun get-shifts (partition)
  (labels ((shift (record)
             (let ((time (get-time record)))
               (if (eql (third time) 0)
                   (fourth time)
                   0))))
    (map 'list #'shift partition)))

(defun summarize (partition)
  (let ((timesheet (minutes-working (get-shifts partition))))
      (list
       :guard (get-guard (first partition))
       :timesheet timesheet
       :total (sum-bit-vector timesheet))))

(defun get-total (x) (getf x :total))
(defun get-asleep (x) (getf x :asleep))
(defun get-timesheet (x) (getf x :timesheet))

(defparameter *summaries*
  (group-by
   (mapcar #'summarize *partitions*)
   :key #'get-guard
   :value (lambda (x)
            (list
             :timesheet(get-timesheet x)
             :total (get-total x)
             :asleep (- 60 (get-total x))))))

(defun sleepiest-worker (summaries)
  (let ((tally
         (mapcar
          (lambda (x)
            (cons (first x) (apply #'+ (mapcar #'get-asleep (rest x)))))
          summaries)))
    (iter (for x in tally) (finding (car x) maximizing (cdr x)))))


(defun sum-timesheets (timesheets)
  (iter (for i from 0 to 59)
                (collect
                    (cons i (iter (for ts in timesheets) (sum (aref ts i)))))))

(defun part-one (summaries)
  (let* ((worker (sleepiest-worker summaries))
         (summary (iter (for x in summaries)
                        (finding (cdr x) such-that (eql (car x) worker))))
         (timesheets (mapcar #'get-timesheet summary))
         (minutes-summary (sum-timesheets timesheets)))
    (* worker
       (iter (for x in minutes-summary)
             (finding (car x) minimizing (cdr x))))))

(format t "Day 4, Part 1: ~A~%" (part-one *summaries*))





(defun sum-timesheets-sleep (timesheets)
  (iter (for i from 0 to 59)
                (collect
                    (cons i (iter (for ts in timesheets) (sum (- 1 (aref ts i))))))))

(defun part-two (summaries)
  (let ((tallies
         (iter
           (for s in summaries)
           (let ((x (first s))
                 (y (rest s)))
             (collect
                 (list :guard x
                       :total (sum-timesheets-sleep (mapcar #'get-timesheet y))))))))

    (iter (for tally in tallies)
          (let ((max-sleep-cons
                 (iter (for x in (getf tally :total))
                       (finding x maximizing (cdr x)))))
            (finding (* (get-guard tally) (car max-sleep-cons))
                     maximizing (cdr max-sleep-cons))))))

(format t "Day 4, Part 2: ~A~%" (part-two *summaries*))
