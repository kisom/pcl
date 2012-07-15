;; code from chapter 4 of Practical Common Lisp, "Syntax and
;; Semantics"
;; http://www.gigamonkeys.com/book/syntax-and-semantics.html

;; instead of the traditional approach to programming languages,
;; wherein there is one "black box" containing all the subsystems
;; (e.g. lexical analyzer, parser, evaluator), common lisp uses two:
;; the reader and the evaluator. in the former model, the data
;; structure used by the evaluator is hidden; in lisp, it's accessible
;; to the programmer.

;; the reader translates strings of characters into s-expressions,
;; which are lisp objects. a consequence of their design (the ability
;; to include lists of arbitrary objects), they can represent
;; arbitrary trees - similar to the AST favoured by most other
;; languages.

;; the evaluator defines a syntax for lisp built from
;; s-expressions. not all s-expressions will be legal.

;; this means s-expression can be used as a serializable data format,
;; and that the language can be changed through programmatic code
;; generation and through manipulating existing data. data is code is
;; data, a concept known as homoiconicity.

;; the basic elements of s-expressions are lists and atoms. lists are
;; delimited by parens, and their elements are themselves
;; s-expressions. the most commonly used atoms are numbers, strings,
;; and names. string literals are enclosed in double quotes, and use
;; the backslash as an escape character.

;; names are represented by objects termed symbols. the reader has no
;; understanding of what a symbol evaluates to. names are converted to
;; uppercase, i.e, format is the same FORMAT. names may be escaped
;; with vertical bars: |foo| evaluates to foo, and not FOO. standard
;; style is to code in lower case and allow the reader to translate to
;; upper case. the reader will intern symbols once it has read their
;; name and converted to uppercase. the reader checks the package for
;; an existing symbol with that name, and if one doesn't exist,
;; creates a new symbol and adds it to the list.

;; there are a few style conventions: hyphens instead of underscores,
;; the use of * to denote global variables, surrounding constants with
;; +, and certain low level functions start with % or %%.

;; the following s-expression
(defun hello-world ()
  (format t "hello, world"))
;; contains four items: a pair of symbols, an empty list, and a list
;; with two symbols and a string.

;;; s-expressions are lisp foms
;; after the reader has translated the text -> s-expressions, the
;; evaluator attempts to evaluate those s-expressions as code. the
;; evaluator may be thought of at this early stage as a function that
;; takes a syntactically-correct lisp form (an s-expression that can
;; be evaluated as lisp code) and returns a value, which is the value
;; of that form.

;; the most basic lisp forms, atoms, may be split into two types:
;; symbols and everything else. symbols are evaluated as the name of a
;; variable (and therefore evaluate to the value of the variable). the
;; rest are self-evaluating; that is, they evaluate to themselves. the
;; variables they name are assigned the value of the symbol
;; itself. keyword symbols similarly evaluate to themselves.

;; lists are evaluated in one of three ways, depending on the starting
;; symbol: function names, macros, and special operators. symbols that
;; haven't been defined yet are assumed to be function names. these
;; three forms are termed function call forms, macro forms, and
;; special forms.

;; function calls have the syntax (function-name argument*); arguments
;; are evaluated and passed to the named function.

;; special forms are those whose arguments are not all evaluated
;; before being passed to the function. for example, the IF operator
;; (if test-form then-form [else-form])
;; only evaluates the then-form when the first argument is true.

;; another special form is QUOTE, which returns its arguments
;; unevaluated. it has a special syntax built into the reader: the '
;; symbol - (quote (+ 1 2)) is the same as '(+ 1 2).

;; macros are the means for extending the language's syntax. they take
;; s-expressions as arguments and return a lisp form evaluated in
;; place of the macro. there are two phases to macro evaluation:
;; the elements of the form are passed to the macro function
;; unevaluated; this form (the macro's expansion) is then evaluated by
;; standard evaluation rules. during compilation, all macros are
;; recursively expanded until the code consists entirely of functions
;; and special forms; this is loaded into a FASL file that can be
;; loaded via the LOAD form. it isn't until evaluation time that this
;; code is run. therefore, macros have a high compilation cost. the
;; forms passed to the macro don't have to be valid lisp forms; the
;; macro may manipulate its arguments to transform them into valid
;; lisp forms. the purpose of macros is to provide a hook into the
;; compiler.

;;; truth, falsehood, and equality
;; the symbol NIL is the only false value and everything else is
;; true. the symbol T is the canonical true value.

;; NIL is a slightly complicated form: it is at once an atom and a
;; list (an empty list). therefore, NIL and the empty list are equivalent.


;;  LocalWords:  evaluator
