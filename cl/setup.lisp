(load "~/.sbclrc")

(ql:quickload :metabang-bind :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :iterate :silent t)
(ql:quickload :group-by :silent t)
(load "aoc.lisp")


(defmacro aoc-package (n)
  (let ((name (alexandria:make-keyword (format nil "AOC2018.DAY~A" n)))
        (packages '(:common-lisp :aoc
                    :cl-ppcre :alexandria :iterate :group-by)))
    `(progn
       (defpackage ,name
         (:use ,@packages))
       (in-package ,name))))


