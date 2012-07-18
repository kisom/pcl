;;;; test suite from Peter Seibel's "Practical Common Lisp"
;;;; code is from chapter 9: practical: Building a Unit Test
;;;; Framework."

;;;; to see an example of the test suite, invoke the RUN-TESTS
;;;; function. you can do this in sbcl from the command line with
;;;; sbcl --load test-suite.lisp --eval '(progn (run-tests) (quit))'

(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body body)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for expr in body collect `(unless ,expr (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  (format t "~a ... ~a:~,8T~:[FAIL~;pass~]~%" *test-name* form result)
  result)

(defmacro check (&body body)
  `(combine-results
    ,@(loop for expr in body collect `(report-result ,expr ',expr))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
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

(deftest test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

(deftest test-math ()
  (combine-results
   (test-arithmetic)))

;; run the test suite
(defun run-tests ()
    (format t "~%running test suite:~%")
    (test-math))
