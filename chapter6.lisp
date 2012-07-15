;;; code from chapter 6 of Practical Common Lisp, "Variables"
;;; http://www.gigamonkeys.com/book/variables.html

;; common lisp uses two types of variables: lexical and dynamic
;; (roughly corresponding to "local" and "global").

;;; variable basics
;; conceptually, all values are references to objects. therefore,
;; assigning a variable updates the reference but does not affect the
;; original value; in the event the variable points to mutable data,
;; the reference can be used to modify that value.

;; in a function definition, the parameter list defines new
;; variables. when the function is called, lip creates new bindings to
;; hold the arguments (a binding is a runtime manifestation of a
;; variable). a variable's binding can change over the course of a
;; program.

;; the LET special operator also introduces variables:
;; (let (variable *)
;;   body-form*)

(let ((x 10) (y 20) z)
  (list x y z))

;; when LET is evaluated, all initial forms are first evaluated. after
;; LET, the variable bindings return to their previous meanings -
;; possibly undefined. the value of the LET form is the last value of
;; the body.

;; the scope of function parameters and LET variables is limited to
;; that form, called the binding form. the innermost bindings shadow
;; the outermost ones.

;; another example is the DOTIMES loop:
(dotimes (x 10) (format t "~d" x))

;; the x is rebound each iteration of the loop.

;; another binding form is LET*; in LET, variable names bound in the
;; LET may only be used in the body whereas in LET*, variable names
;; bound in the LET* may be used in later variable bindings. for
;; example,
(let* ((x 10)
        (y (+ x 10)))
     (list x y))

;;whereas
;; (let ((x 10)
;;       (y (+ x 10)))
;;   (list x y))
;; is not valid. we would have to employ nested LETs:
(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

;;; lexical variables and closures
;; lisp defaults to lexically scope variables; i.e. limited to the
;; binding form. what happens when an anonymous function contains a
;; reference to a lexical variable from an enclosing scope?
(let ((count 0)) #'(lambda () (setf count (1+ count))))

;; the binding of count will stick around as long as needed. this
;; anonymous function is called a closure, as it "closes over" the
;; lexical binding from LET. it is important to note that it is the
;; binding and not the value captured: new values can be reassigned
;; that will persist. for example, the closure may be captured in a
;; global variable:
(defparameter *count-fn* (let ((count 0))
                           #'(lambda () (setf count (1+ count)))))
  (funcall *count-fn*)     ;; 1
(funcall *count-fn*)       ;; 2

;; a single closure can close over many bindings; multiple closures
;; may close over the same binding.

;;; dynamic ("special") variables
;; lexical variables improve code readability by limiting scope;
;; however, they are not always appropriate.

;; lisp provides two mechanisms for creating global variables: DEFVAR
;; and DEFPARAMETER. Both have the form
;; (defvar name initial-value [optional-docstring])
;; variables created this are traditionally surrounded in *'s.  DEFVAR
;; will only assign the value if the variable is undefined, while
;; DEFPARAMETER always assigns the value. It is possible to DEFVAR a
;; name with no value, creating an unbound variable.

;; DEFVAR should be used with variables that should persist despite
;; code changes.

;; dynamic variables allow us to temporarily modify global variables,
;; properly restoring them when done. all global variables are dynamic
;; variables. for example,
;; (let ((*standard-output* *some-stream*))
;;   (more-code))
;; references to *standard-output* in more-code will use the
;; temporarily shadowed value; outside the LET, *standard-output*
;; retains its original value. conceptually, this is done via a stack
;; of bindings. an ilustration:
(defvar *x* 10)
(defun foo% () (format t "X: ~d~%" *x*))
(foo%)                  ; prints 10
(let ((*x* 20)) (foo%)) ; prints 20
(foo%)                  ; prints 10
(defun bar% ()
  (foo%)
  (let ((*x* 20)) (foo%))
  (foo%))

;; let's see what happens when foo contains an assignment to *x*
(defun foo%% ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (incf *x*))
  (format t "After assignment~18tX: ~d~%" *x*))
(foo%%)  ;; 10, 11
(bar%)   ;; 11, 12, 20, 21, 12, 13

;; the SETF inside the let used the dynamic binding of *x*
;; the naming convention becomes important: if we think we're using a
;; lexical variable when we are instead using a dynamic variable, it
;; could have unintended consequences. similarly, we should only use
;; dynamic variables when we need the ability to change behaviour of
;; downstream code or receive modifications from downstream code.

;;; constants
;; (defconstant name initial-value-form [ documentation-string ])
;; the convention is name constants as +name+. the constant may be
;; redefined by reevaluating a DEFCONSTANT, but the behaviour is
;; implementation specific; we may have to reevaluate code using it.

;;; assignment
;; the SETF macro is used for assignment:
;; (setf place value)
;; it expands to a call to SETQ, which as a special operator has
;; access to both lexical and dynamic bindings. SETF returns the value
;; assigned, and calls may be nested:
(defparameter *y* 0)
(defparameter *z* 0)
(setf *y* (setf *z* (random 10)))
(format t "y: ~d~10tz: ~d~%" *y* *z*)   ; *y* and *z* are the same
                                        ; number now

;; SETF can also assign to multiple places in sequence:
(setf *y* 1 *z* 2)
(format t "y: ~d~10tz: ~d~%" *y* *z*)

;;; generalised assignment
;; SETF can assign any place a value. SETFing a place that is part of
;; a larger object has the same semantics as SETFing a variable.

;;; other ways to modify places
;; incrementing and decrementing a number may be done with INCF and
;; DECF. they are examples of modify macros, macros built on top of
;; SETF. in general, they are guaranteed to evaluate arguments and
;; subforms left to right and only once.

;; PUSH is another modify macro, as are ROTATEF:
;; remember, *y* is now 1 and *z* is 2:
(rotatef *y* *z*)
(format t "rotatef: y: ~d~10tz: ~d~%" *y* *z*)

;; SHIFT shifts to the left:
(shiftf *y* *z* 5)
(format t "shiftf: y: ~d~10tz: ~d~%" *y* *z*)
