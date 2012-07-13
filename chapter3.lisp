;;; code from chapter 3 of Practical Common Lisp
;;; http://www.gigamonkeys.com/book/practical-a-simple-database.html

;; property lists (aka plist)
;; list of pairs, where first element is a symbol describing the next
;; element. for the CD database example from the book, we'll use a
;; keyword symbol:
(list :a 1 :b 2 :c 3)

;; values are retrieved using the GETF function:
(getf (list :a 1 :b 2 :c 3) :a)

;; now we write a function to make a suitable list for the CD
;; database:
(defun make-cd (title artist rating rippedp)
  (list :title title :artist artist :rating rating :rippedp rippedp))

;; we need a larger construct to store the list of albums. for
;; simplicity's sake in this example, we'll use a global variable. the
;; convention is for global variables to have "earmuffs",
;; i.e. *varname*.
(defvar *db* nil)

;; adding records is done via the push macro, which we'll abstract a
;; bit:
(defun add-record (cd) (push cd *db*))

;; a more attractive way of viewing the output than just evaluating
;; *db* on the REPL. the format statement can be used to display a
;; prettier listing:
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;; the dolist macro steps through each value of of *db*, binding each
;; element in turn to the cd symbol. format will be examined in depth
;; in the future. for now, know the first argument is the output
;; stream (and t is shorthand for the *standard-output* stream). the
;; second argument is a format string; format directives start with ~:
;;     ~a        aesthetic directive; consume an argument and display
;;               it in a human-readable format.
;;     ~t        tabulate; ~nt emits enough spaces to move to the nth
;;               column. no arguments are consumed.
;;     ~%        newline; no arguments are consumed.
;;     ~{        start processing a list
;; each pass through the loop will consume a keyword and element.
;; we could have turned the dump-db into a one-liner:
(defun dump-db-1l ()
    (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; the dolist variant is a bit easier to read, however, and the code
;; is a bit clearer as to what it idoes.
