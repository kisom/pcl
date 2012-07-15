;;; code from chapter 5 of Practical Common Lisp, "Functions"
;;; http://www.gigamonkeys.com/book/functions.html

;; the three most basic components of all lisp programs are functions,
;; variables and macros. macros are the core of the lisp way: even
;; macros expand down to functions.

;; basic syntax for defining a function:
;; (defun name (parameters*)
;;   "Optional doc string."
;;    body-form*)

;; the doc string can be looked up with the DOCUMENTATION
;; function. the body may consist of any number of forms; the value of
;; the last is returned. alternatively, the RETURN-FROM special
;; operator can be used to return immediately.

;;; function parameter lists
;; some functions do not require all the parameters; some may be
;; optional and can be set to a safe default value. to define optional
;; parameters, use the symbol &optional before the parameters. for
;; example:
;; (defun foo (a b &optional c d) (list a b c d))
;; the parameters c and d do not have default values, and therefore
;; default to nil:
;; (foo 1 2)     ==> (1 2 NIL NIL)
;; (foo 1 2 3)   ==> (1 2 3 NIL)
;; (foo 1 2 3 4) ==> (1 2 3 4)

;; to assign a default value, replace the simple parameter name
;; symbol with a list followed by the name:
;; (defun foo (a &optional (b 10)) (list a b))
;; the default value can refer to previously-defined parameters:
;; (defun make-rectangle (width *optional (height width)) ...)

;; we can also provide a supplied parameter to determine if the value
;; passed in was nil or the default value of nil was passed in.

;; functions can also use the &rest symbol.

;;; keyword parameters
;; use &key:
;; (defun foo (&key a b c) (list a b c))
;; (foo)                ==> (NIL NIL NIL)
;; (foo :a 1)           ==> (1 NIL NIL)
;; (foo :b 1)           ==> (NIL 1 NIL)
;; (foo :c 1)           ==> (NIL NIL 1)
;; (foo :a 1 :c 3)      ==> (1 NIL 3)
;; (foo :a 1 :b 2 :c 3) ==> (1 2 3)
;; (foo :a 1 :c 3 :b 2) ==> (1 2 3)
;; keyword parameters follow the default value semantics.

;; parameter names must be specified in the order of required
;; parameters, optional parameters, rest parameter, and finally the
;; keyword parameters. combining optional or rest parameters with
;; keyword parameters can be interesting because the keyword arguments
;; can be eaten up keywords and values intended for the keyword
;; parameters. it's safer to just create default values for keyword
;; parameters.

;; if &rest is used with &key is used, the keyword parameters are
;; gathered into the rest argument and are assigned to keyword values:
;; (defun foo (&rest rest &key a b c) (list rest a b c))
;; (foo :a 1 :b 2 :c 3)  ==> ((:A 1 :B 2 :C 3) 1 2 3)

;;; function return values
;; RETURN-FROM is used to return from a block of code defined with
;; BLOCK special operator. functions automatically wrap their body in
;; a block, however. as RETURN-FROM takes the name of the block it is
;; returning from (which isn't quoted as it isn't evaluated), using it
;; in a function requires the function name it is returning from as
;; the first argument.

;; (defun foo (n)
;;   (dotimes (i 10)
;;     (dotimes (j 10)
;;       (when (> (* i j) n)
;;         (return-from foo (list i j))))))

;;; functions as data: higher order functions
;; it is sometimes useful to treat functions as data. anonymous
;; functions can be defined with LAMBDA. FUNCTION is a special
;; operator providing access to a function object, with the syntactic
;; sugar #' provided as an alternate form.
