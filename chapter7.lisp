;;; code from chapter 7 of Practical Common Lisp, "Macros: Standard
;;; Control Constructs""
;;; http://www.gigamonkeys.com/book/macros-standard-control-constructs.html

;; the model of core language + standard library (written in the
;; language) means the language is easy to extend, to write the
;; language we need v. the language we've been given.

;;; when and unless
;; IF is the most basic conditional execution form, but it is limited:
;; it is restricted to a single form for each clause. the special
;; operator PROGN lets us do this:
;; (if (spam-p current-message)
;;     (progn
;;       (file-in-spam-folder current-message)
;;       (update-spam-database current-message))))
;; however, there's a better construct: WHEN
;; (when (spam-p current-message)
;;       (file-in-spam-folder current-message)
;;       (update-spam-database current-message))

;; if WHEN wasn't built in, it could be defined with a simple macro:
(defmacro new-when (condition &rest body)
  `(if ,condition (progn ,@body)))

;; the counterpart to WHEN is UNLESS:
(defmacro new-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; even these trivial examples demonstrate how easy it is to adapt the
;; syntax to improve the program's clarity.

;;; cond
;; if is also ugly when it comes to multibranch conditionals:
;; (if a
;;     (do-x)
;;     (if b
;;         (do-y)
;;         (do-z)))

;; cond is a macro that clarifies multibranch conditionals:
;; (cond (a (do-x))
;;       (b (do-y))
;;       (t (do-z)))

;;; AND, OR, and NOT
;; NOT is a function, whereas AND and OR are macros.

;;; looping constructs
;; none of lisp's 25 special operators directly support structured
;; looping.

;; the lowest level construct is DO, which tends to be overkill for
;; simple things. DOLIST and DOTIMES are provided as convenience
;; macros for common cases. LOOP provides a heavyweight Algol-like
;; language for expressing loop constructs. some people like it
;; because it is concise and expressive; others dislike it for not
;; being lispy enough.

;;; DOLIST and DOTIMES
;; (dolist (var list-form)
;;   body-form*)

;; when the loop starts, the list-form is evaluated one to produce a
;; list; each iteration of the loop, var is set to the next item in
;; the list and body is evaluated:
(dolist (x '(1 2 3))
  (print x))

;; used this way, the DOLIST form evaluates to NIL. a DOLIST can be
;; broken out of using RETURN:
(dolist (x '(1 2 3))
  (print x)
  (if (evenp x) (return)))

;; DOTIMES is a counting loop, with a similar basic syntax to DOLIST:
;; (dotimes (var count-form)
;;   body-form)
;; count-form must evaluate to an integer, the loop runs from 0 to
;; n - 1. early return with RETURN is also supported. as the bodies of
;; DOLIST and DOTIMES support any type of expression, nested loops are
;; possible:
(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

;;; DO
;; while DOLIST and DOTIMES provide good expressivity for common
;; cases, there are cases where more flexibility is required. the
;; syntax for DO is:
;; (do (variable-definition*)
;;     (end-test-form result-form*)
;;   statement*)

;; a variable definition consists of:
;; (var init-form step-form)

;; init-form is evaluated at the beginning of the loop; at each
;; iteration, step-form is evaluated and used to set the var. without
;; a step-form, the variable keeps its init-form unless set in the
;; body. if init-form is left out, the variable is set to NIL. at the
;; beginning of each iteration, end-test-form is evaluated - if it
;; evaluates to NIL, the iteration continues. when it evaluates to T,
;; the result-forms are evaluated, and the result of the last form is
;; returned as the result of the DO form. at the beginning of each
;; iteration, all variables are evaluates before assigning values;
;; that is, the variable section behaves like LET*. for example,
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
;; this example also illustrates that because multiple variables may
;; be stepped, often times a body isn't needed. here is example of a
;; DO that binds no variables:
(format t "setting future time...~%")
(defparameter *some-future-date* (+ 61 (get-universal-time)))
(format t "begin DO loop...~%")
(do ()
    ((> (get-universal-time) *some-future-date*))
  (format t "waiting~%")
  (sleep 60))

;;; the mighty LOOP
;; there are two flavours: simple and extended. the simple form:
;; (loop body-form*)
;; this iterates forever until RETURN is used to break out. the
;; previous DO could be written with a LOOP:
(format t "setting future time...~%")
(defparameter *some-future-date* (+ 61 (get-universal-time)))
(format t "begin LOOP...~%")
(loop
     (when (> (get-universal-time) *some-future-date*)
       (return))
   (format t "waiting~%")
   (sleep 60))

;; the extended syntax is a bit more complex. compare this idiomatic
;; DO loop collecting numbers from 1 to 10 into a list:
(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))

;; with this LOOP version:
(loop for i from 1 to 10 collecting i)

;; summing up first ten squares:
(loop for x from 1 to 10 summing (expt x 2))

;; counting vowels in a string
(loop for x across "the quick brown jumps over the lazy dog"
     counting (find x "aeiou"))

;; the extended loop syntax is discussed in chapter 22. while LOOP is
;; a far more complicated macro than WHEN or UNLESS, it is still just
;; a macro.
