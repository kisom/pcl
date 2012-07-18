;;; code from chapter 8 of Practical Common Lisp, "Practical: Building
;;; a Unit Test Framework""
;;; http://www.gigamonkeys.com/book/macros-standard-control-constructs.html

;; functional programs are easy to test: the lack of side effects
;; means the result of the function can be tested directly. when side
;; effects are introduced, the behaviour of the function has to be
;; checked via both its return value and its effects.

;; we want something as simple as (test= expr result), and to be able
;; to run multiple cases through the test.

;; to prevent code duplication, we need a function to output the test
;; result:
(defun report-result% (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

;; for this to work we'll want to be able to treat expressions as both
;; code and (for the test result) and data (for the display). a desire
;; to treat code as data is a sign it's time to use a macro. if we
;; write something like
;; (check (= (+ 1 2) 3))

;; this should expand to:
;; (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))

;; this is a fairly trivial macro:
(defmacro check% (expr)
  `(report-result% ,expr ',expr))

(macroexpand-1 '(check% (= (+ 1 2) 3)))
;; (REPORT-RESULT (= (+ 1 2) 3) '(= (+ 1 2) 3))
;; T

;; while we're writing this, might as well remove the need for
;; multiple calls to check:
(defmacro check%% (&body body)
  `(progn
     ,@(loop for expr in body collect `(report-result% ,expr ',expr))))

;; now, if we want our test function to return T if all tests passed,
;; report-result will need to be modified to return the result:

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

;; to collect these, we can't just substitute AND for PROGN: AND will
;; short circuit, and this is typically not the behaviour we
;; want. we can, however, write a macro to solve this. we'll call it
;; combine-results and it will be used like:
;; (combine-results
;;   (foo)
;;   (bar)
;;   (baz))

;; to avoid a leak, we'll use a temporary name via the with-gensyms
;; macro. for completeness, here is the definition from the last
;; chapter:
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body body)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for expr in body collect `(unless ,expr (setf ,result nil)))
       ,result)))

(defmacro check (&body body)
  `(combine-results
    ,@(loop for expr in body collect `(report-result ,expr ',expr))))

;; verifying for #'+:
(defun test-+% ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

;; pass ... (= (+ 1 2) 3)
;; pass ... (= (+ 1 2 3) 6)
;; pass ... (= (+ -1 -3) -4)
;; T

;; and if we change one of the tests to fail:
(defun test-+-fails ()
  "Failurizing version of the demo test suite."
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 5)
    (= (+ -1 -3) -4)))

;;; better result reporting
;; as we begin developing larger suites, we'll want to know where
;; failures occur:
(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
   (test-+%)
   (test-*)))

;; we need a way to represent which test function we're in to figure
;; out where errors are occuring; dynamic variables let us do this
;; without having to pass a large amount of extra information between
;; functions. so now, we'd create a dynamic variable at the top of the
;; source file:
(defvar *test-name* nil)

;; now we update report-result to include the test name:
(defun report-result (result form)
  (format t "~a ... ~a:~,8T~:[FAIL~;pass~]~%" *test-name* form result)
  result)

;; now include a LET in the tests:
(defun test-+% ()
  (let ((*test-name* 'test-+%))
   (check
     (= (+ 1 2) 3)
     (= (+ 1 2 3) 6)
     (= (+ -1 -3) -4))))

(defun test-*% ()
  (let ((*test-name* 'test-*))
   (check
     (= (* 2 2) 4)
     (= (* 3 5) 15))))

;; now we can re-evaluate test-arithmetic:
(test-arithmetic)
;; TEST-+% ... (= (+ 1 2) 3): pass
;; TEST-+% ... (= (+ 1 2 3) 6): pass
;; TEST-+% ... (= (+ -1 -3) -4): pass
;; TEST-*% ... (= (* 2 2) 4): pass
;; TEST-*% ... (= (* 3 5) 15): pass
;; T

;;; an abstraction emerges
;; having to use the name twice: once in the defun and once in the let
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

;; and perhaps now we rerun test-arithmetic. we should get the same
;; output.
(test-arithmetic)

;; to get full test hierarchy, we can use *test-name* as a list:
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; tests need to be re-evaluated to make use of new deftest:
(deftest test-+ ()
  (check
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
   (= (* 2 2) 4)
   (= (* 3 5) 15)))


;; and redefine TEST-ARITHMETIC as a test:
(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

;; output now includes full test hierarchy
(format t "~%VERIFY TEST NAME CONTAINS FULL HIERARCHY~%")
(test-arithmetic)

;; more test hierarchy examples:
(deftest test-math ()
  (test-arithmetic))

;; the complete test suite is in test-suite.lisp
