(load "~/.sbclrc")

(ql:quickload :metabang-bind :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :iterate :silent t)
(ql:quickload :group-by :silent t)
(ql:quickload :cl-utilities :silent t)
(ql:quickload :cl-arrows :silent t)
(ql:quickload :incf-cl :silent t)

(load "aoc.lisp")


(defmacro aoc-package (n)
  (let ((name (alexandria:make-keyword (format nil "AOC2018.DAY~A" n)))
        (packages '()))
    `(progn
       (defpackage ,name
         (:use :common-lisp :aoc
               :cl-ppcre :alexandria :iterate :group-by
          :cl-arrows)
         (:import-from :cl-utilities :split-sequence)
         (:import-from :incf-cl :take :drop :split-at))
       (in-package ,name))))
