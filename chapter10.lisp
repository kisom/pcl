;;; code from chapter 10 of Practical Common Lisp, "Numbers,
;;; Characters and Strings"
;;; http://www.gigamonkeys.com/book/numbers-characters-and-strings.html

;; from fred brooks in _the mythical man month_, "representation _is_
;; the essence of programming."

;; from the point of view of the language's user, the built-in data
;; types are defined by their attendant functions; therefore, to
;; understand the data type, understand the functions.

;; strings are a little weird because although they are sequences of
;; characters, many functions operate on them as a scalar value.
