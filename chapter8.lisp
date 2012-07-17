;;; code from chapter 8 of Practical Common Lisp, "Macros: Defining
;;; Your Own""
;;; http://www.gigamonkeys.com/book/macros-defining-your-own.html

;; macros exist to allow programmers to create abstractions on top of
;; the core library and standard library.

;;; macro expansion time vs. runtime
;; the key to understanding macros is to understand the distinction
;; between code that generates code (macros) and the code that
;; eventually makes up the program. at macro expansion time, there is
;; no way to access the data that'll exist at runtime. the job of a
;; macro is to produce code, not to do anything directly. the basic
;; syntax for DEFMACRO:
;; (defmacro name (parameter*)
;;   "Optional documentation string."
;;   body-form*)

;; the first step of a macro is to write at least one example of a
;; call to the macro and the code it should expand into. it's
;; important to make sure the macro doesn't leak details of its
;; implementation.

;; in summary, the steps to writing a macro:
;; 1. write a sample call to the macro and the code it should expand
;; to
;; 2. write code that generates the handwritten expansion from the
;; arguments in the sample call.
;; 3. make sure the macro abstraction doesn't "leak."

;;; sample macro: do-primes
;; a dolist that iterates over a series of successive prime numbers

;; the first step is to write two utility functions: a primality test
;; and another to return the next prime number greater than its
;; argument:
(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

;; now we write an example of the call:
;; (do-primes (p 0 19)
;;   (format t "~d" p))

;;; macro parameters
;; the first step in a macro is to extract the parts of the objects
;; needed for the expansion. for do-primes, the first argument is a
;; list containing the name of the loop variable and the rest to hold
;; the body. a first pass might be:
(defmacro do-primes% (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

;; however, macro parameter lists are destructuring; furthermore, we
;; can use &body can be used as a synonym for &rest. we can use this
9;; to streamline the macro:
(defmacro do-primes%% ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;; destructuring also provides automatic error checking: a call with
;; the first argument that is not a three-element list. these
;; parameter lists can also make use of &optional, &key, and &rest.

;;; generating the expansion
;; the current version of do-primes doesn't quite handle edge cases
;; well. for the original example, we'll want to verify it's
;; correct. there are two ways to test: use it:
(do-primes%% (p 0 19) (format t "~d " p))
;; => 2 3 5 7 11 13 17 19
;; or check the expansion:
(macroexpand-1 '(do-primes%% (p 0 19) (format t "~d " p)))
;; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
;;     ((> P 19))
;;   (FORMAT T "~d " P))
;; T
;; it appears do-primes works according to our description before.

;;; plugging the leaks
;; there are three ways macros can leak; and the current definition
;; suffers from the one. it evaluates the end subform too many
;; times. for example, if an expression such as (random 100) was used
;; instead of the a literal number we end up with an issue:
(macroexpand-1 '(do-primes%% (p 0 (random 100))
                 (format t "~d " p)))

;; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P))))
;;     ((> P (RANDOM 100)))
;;   (FORMAT T "~d " P))
;; T

;; RANDOM is called each iteration. a first pass at fixing:

(defmacro do-primes%%% ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

;; this introduces two new leaks: the initialisation forms are
;; evaluated in the order they are defined. therefore, end is
;; evaluated before the expression passed as start. if they are forms
;; that have side effects, this can have unintended consequences. this
;; is trivial to fix:

(defmacro do-primes%%%% ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;; the other problem is the fact that the name ending-value may shadow
;; names passed in:
;; (do-primes%%%% (ending-value 0 10) (print ending-value))

;; we can use the GENSYM function to generate a name:
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(macroexpand-1 '(do-primes (p 0 (random 100)) (format t "~d " p)))
;; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))
;;      (#:G920 (RANDOM 100)))
;;     ((> P #:G920))
;;   (FORMAT T "~d " P))
;; T

;; not all literal names used in a macro expansion will cause
;; problems, but it's often safest to use GENSYM. with time, it'll
;; become easier to write plugged macros. peter gives the following
;; rules of thumb:

;; * Unless there's a particular reason to do otherwise, include any
;;   subforms in the expansion in positions that will be evaluated in
;;   the same order as the subforms appear in the macro call.
;; * Unless there's a particular reason to do otherwise, make sure
;;   subforms are evaluated only once by creating a variable in the
;;   expansion to hold the value of evaluating the argument form and
;;   then using that variable anywhere else the value is needed in the
;;   expansion.
;; * Use GENSYM at macro expansion time to create variable names used
;;   in the expansion.

;;; macro-writing macros
;;  certain macro patterns come up repeatedly that may be able to be
;;  abstracted with other macros. as an example, we'll write
;;  with-gensyms, a macro that expands into a LET where each of its
;;  arguments refers to a genysm'd symbol. this will let us write
;;  something like:
(defmacro new-do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;; new-do-primes expands to
(macroexpand-1 '(new-do-primes (p 0 (random 100)) (format t "~d " p)))

;; (DO ((P (NEXT-PRIME 0) (NEXT-PRIME (1+ P)))
;;      (#:G929 (RANDOM 100)))
;;     ((> P #:G929))
;;   (FORMAT T "~d " P))
;; T

;;; another classic macro-writing macro: ONCE-ONLY
;; generates code that evaluates certain macro arguments once only and
;; in a certain order:
(defmacro once-only-do-primes ((var start end) &body body)
  (once-only (start end)
             `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
                  (( > ,var ,end))
                ,@body)))

;; this is a fairly complicated macro, relying on multiple levels of
;; backquoting and unquoting:
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
