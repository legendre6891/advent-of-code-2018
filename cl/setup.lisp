(load "~/.sbclrc")

(ql:quickload :metabang-bind :silent t)
(ql:quickload :cl-ppcre :silent t)
(ql:quickload :alexandria :silent t)
(ql:quickload :iterate :silent t)
(ql:quickload :group-by :silent t)
(ql:quickload :cl-utilities :silent t)
(ql:quickload :cl-arrows :silent t)
(ql:quickload :incf-cl :silent t)
(ql:quickload :fset :silent t)
(ql:quickload :trivia :silent t)
(ql:quickload :recur :silent t)

(setf trivia.level2:*arity-check-by-test-call* nil)

(load "aoc.lisp")


(defmacro aoc-package (n)
  (let ((name (alexandria:make-keyword (format nil "AOC2018.DAY~A" n))))
    `(progn
       (defpackage ,name
         (:use :common-lisp :aoc
               :cl-ppcre :alexandria :iterate :group-by
               :cl-arrows :fset :gmap
               :new-let :lexical-contexts :recur)
         (:shadowing-import-from :trivia #:match)
         (:shadowing-import-from :new-let #:let #:cond)
         (:shadowing-import-from :alexandria :removef)
         (:shadowing-import-from
          :fset
	  ;; Shadowed type/constructor names
	  #:set #:map
	  ;; Shadowed set operations
	  #:union #:intersection #:set-difference #:complement
	  ;; Shadowed sequence operations
	  #:first #:last #:subseq #:reverse #:sort #:stable-sort
	  #:reduce
	  #:find #:find-if #:find-if-not
	  #:count #:count-if #:count-if-not
	  #:position #:position-if #:position-if-not
	  #:remove #:remove-if #:remove-if-not
	  #:substitute #:substitute-if #:substitute-if-not
	  #:some #:every #:notany #:notevery
          ;; conflicts with iterate
          #:with
          ;; conflicts with alexandria
          #:compose #:appendf #:unionf)
         (:import-from :cl-utilities :split-sequence)
         (:import-from :incf-cl :take :drop :split-at))
       (in-package ,name))))
