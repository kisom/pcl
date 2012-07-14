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

;; let's add a function to get better input from the user.
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; we force output because some implementations won't print until they
;; get a newline. read-line will read a single line of text. we can
;; use this as the building block for building an album:
;; (defun prompt-for-cd ()
;;   (make-cd
;;    (prompt-read "Title")
;;    (prompt-read "Artist")
;;    (prompt-read "Rating")
;;    (prompt-read "Ripped [y/n]"))))
;; this is commented because strings are not appropriate for the last
;; two fields. to get an integer value, we can use the parse-integer
;; form. by passing the optional keyword argument :junk-allowed, it
;; will relax on errors. we also want to make sure it returns 0, and
;; not the default of nil, if nothing could be read:
;;    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
;; as for the ripped, CL provides the form y-or-n-p:
;;    (y-or-n-p "Ripped [y/n]: ")
;; putting the pieces together:

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

;; finally, we can wrap this in a function to keep adding albums until
;; the user is done:
(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Add another? [y/n]: ")) (return))))

;; hacky database persistence
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

;; WITH-OPEN-FILE is a macro that opens a file, binds stream to a var,
;; executes a set of instructions, and closes file. the important
;; thing is that it closes the file even on error. the PRINT form is
;; used instead of FORMAT because it will output in a format that is
;; suitable for reading back with the lisp reader. the
;; WITH-STANDARD-IO-SYNTAX sets certain variables that control the
;; behaviour of PRINT are set to their defaults.

;; restoring the database:
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; direction defaults to input; READ is the same reader used by the
;; REPL - in this case, the input is being read and stored to a
;; variable instead of evaluated. SETF is common lisp's primary
;; assignment operator, and sets its first argument to the result of
;; evaluating the second.

;; querying can be done with the REMOVE-IF-NOT function, which takes a
;; predicate and list. REMOVE-IF-NOT creates a new list, and is an
;; immutable function. the predicate should take a single argument and
;; return a boolean value. note that LAMBDA is a special form for
;; defining an anonymous function.
(defun select (attribute value)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd attribute) value))
   *db*))

;; note that the lambda doesn't pre-evaluate the attribute value; it
;; derives part of its meaning from the context in which it is
;; embedded.

;; keyword arguments can be set up using &key:
;; (defun foo
;;     (&key a b c)
;;   (list a b c))

;; (foo :a 1 :b 2 :c 3)  ==> (1 2 3)
;; (foo :c 3 :b 2 :a 1)  ==> (1 2 3)
;; (foo :a 1 :c 3)       ==> (1 NIL 3)
;; (foo)                 ==> (NIL NIL NIL)

;; the simple arguments in keyword parameters may be modified; their
;; full form is something like
;;    (key default-value supplied-p)
;; supplied-p will return T if the argument was supplied and NIL if
;; not, to allow differentiating between NIL values passed in and a
;; lack of keys. for example, given
;; (defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
;;
;; you would get results such as:
;; (foo :a 1 :b 2 :c 3)  ==> (1 2 3 T)
;; (foo :c 3 :b 2 :a 1)  ==> (1 2 3 T)
;; (foo :a 1 :c 3)       ==> (1 20 3 T)
;; (foo)                 ==> (NIL 20 30 NIL)

;; we'll write a selector function that is more sql-y:
;; (select (where :artist "Tycho"))
(defun where (&key title artist rating (rippedp nil ripped-supplied))
  #'(lambda (cd)
      (and
       (if title           (equal (getf cd :title) title) t)
       (if artist          (equal (getf cd :artist) artist) t)
       (if rating          (equal (getf cd :rating) rating) t)
       (if ripped-supplied (equal (getf cd :rippedp) rippedp) t))))

;; and the appropriate select:
(defun select-album (selector-fn)
  (remove-if-not selector-fn *db*))

;; another feature databases need is the ability to update
;; records. for example, when we get around to ripping a CD or decide
;; that we like the album more (or less) than we originally did, we'll
;; want to update the database to reflect that. for this, we'll use
;; the MAPCAR function to map over a list, returning a new list
;; containing the results of calling the function on each item in the
;; original list.
(defun update-db (selector-fn &key title artist rating
                                (rippedp nil ripped-supplied))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title           (setf (getf row :title) title))
               (if artist          (setf (getf row :artist) arist))
               (if rating          (setf (getf row :rating) rating))
               (if ripped-supplied (setf (getf row :rippedp) rippedp)))
             row) *db*)))

;; SETF can set places besides a variable; in this case, SETF will the
;; value assigned to the relevant keyword.

;; we can also remove albums:
(defun delete-album (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; removing duplication: there are a lot of instances where we have
;; code with a body of clauses such as
;; (if title (equal (getf cd :title) title) t)
;; if we have to change the logic, it has to be updated in a number of
;; places. for example, if we determined that it was spending too much
;; checking whether title and so forth were set? if we hand optimised
;; it, it might look like:

;; (select (where :title "Give Us a Break" :ripped t))

;; you could change it to this:

;; (select
;;  #'(lambda (cd)
;;      (and (equal (getf cd :title) "Give Us a Break")
;;           (equal (getf cd :ripped) t))))

;; alternatively, we can use macros to dynamically generate the
;; code. we'll start with trivial examples and move to replacing
;; the where function with a where macro.

;; this macro takes a list and reverses it:
(defmacro backwards (expr) (reverse expr))
;; example: (backwards ("Hello, world" t format))

;; the macro lets us define new language features at run time.

;; for example, supposed we look at the (equal (getf cd field) value)
;; we could try to wrap this in a function:
;; (defun make-comparison-expr) (field value)    ; wrong
;;   (list equal (list getf cd field) value))

;; because lisp will try to evaluate these simple names (which is fine
;; for field and value, but not for equal and getf), we would need to
;; quote them:
;; (defun make-comparison-expr (field value)
;;   (list 'equal (list 'getf 'cd field) value))

;; similarly, a backquote before an expression stops evaluate just
;; like a forward quote, with any expressions preceded by a comma will
;; be evaluated:
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

;; the old hand-optimised version used an and to collect the
;; predicates together. if we pass arguments as a single list, we can
;; write a function to take pairs of arguments and collect the results
;; together. the LOOP macro will come in handy:
(defun make-comparisons-list (fields)
  (loop while fields
       collecting (make-comparison-expr (pop fields) (pop fields))))

;; LOOP is discussed later; POP is the inverse of PUSH.
;;
;; we can wrap this list in an AND and anonymous function. we'll need
;;to interpolate the return list:
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

;; the ,@ is the splice operator, and splices the value of the
;; expression that follows it into the enclosing list. this means that
;; the expression passed to ,@ must be evaluate to a list.

;; for example:
;; `(and ,(list 1 2 3))   ==> (AND (1 2 3))
;; `(and ,@(list 1 2 3))  ==> (AND 1 2 3)
;; `(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4)

;; the &rest allows the function to take an arbitrary number of
;; arguments in the parameter list; they are collected into a list.
;; for example, the arguments for (where :title "Kid A" :rippedp t)
;; are (:title "Kid A" :rippedp t)

;; MACROEXPAND-1 will do one expansion of a macro, so we can see what
;; the compiler sees:
(macroexpand-1 '(where :title "Kid A" :rippedp t))
;; #'(LAMBDA (CD)
;;     (AND (EQUAL (GETF CD :TITLE) "Kid A") (EQUAL (GETF CD :RIPPEDP) T)))
;; T
