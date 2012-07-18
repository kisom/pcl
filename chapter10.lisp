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

;;; numbers
;; lisp is a good language for math: it supports big numbers,
;; rationals, floating point numbers, and complex numbers.

;; #O and #o indicate octal numbers; #X and #x indicate hexadecimal
;; #numbers; #nR for numbers of base n where 2 <= n <= 36. complex
;; #numbers are written as #C or #c followed by a list of two real
;; #numbers representing the real and imaginary components.

;;; basic math functions
;; FLOOR: truncates down toward negative infinity
;; CEILING: truncate towards positive infinity
;; TRUNCATE: truncates towards zero
;; ROUND: rounds to nearest integer
;; MOD and REM return modulus and remainder
;; 1+ and 1- are shorthand increment and decrement operators that
;; return new values. INCF and DECF are similar but modify a place
;; (they are SETF macros).

;;; numeric comparisons
;; = is the equality operator for numbers. if called with multiple
;; arguments, it only returns true if all are equivalent. /= is the
;; converse.

;; MIN and MAX grab the minimum and maximum values. ZEROP, MINUSP, and
;; PLUSP all test a number's relation to zero; and EVENP and ODDP test
;; a number's oddity.

;;; higher math
;; LOG -> logarithms
;; EXP/EXPT -> exponentiation
;; trig: SIN/COS/TAN, ASIN/ACOS/ATAN, SINH/COSH/TANH,
;; ASINH/ACOSH/ATANH

;;; characters
;; CL standard doesn't mandate a representation for characters, as
;; they are separate from numbers. read syntax for characters:
;; #\<character>. technically, you can do #\ , but better to do
;; #\Space (same for Tab, Page, Rubout, Linefeed, Return, and
;; Backspace).

;;; character comparisons
;; CHAR= is a case sensitive comparison, and CHAR-EQUAL is the
;; case-insensitive version.

;; Character Comparison Functions
;; Numeric Analog Case-Sensitive Case-Insensitive
;; =              CHAR=          CHAR-EQUAL
;; /=             CHAR/=         CHAR-NOT-EQUAL
;; <              CHAR<          CHAR-LESSP
;; >              CHAR>          CHAR-GREATERP
;; <=             CHAR<=         CHAR-NOT-GREATERP
;; >=             CHAR>=         CHAR-NOT-LESSP

;;; strings
;; one dimensional character arrays, but have literal syntax:
;; Numeric Analog Case-Sensitive Case-Insensitive
;; =              STRING=        STRING-EQUAL
;; /=             STRING/=       STRING-NOT-EQUAL
;; <              STRING<        STRING-LESSP
;; >              STRING>        STRING-GREATERP
;; <=             STRING<=       STRING-NOT-GREATERP
;; >=             STRING>=       STRING-NOT-LESSP

;; string comparisons only work on pairs of strings. there are keyword
;; arguments that support substring restrictions: start/end 1 and
;; 2. for example:
(string= "foobarbaz" "quuxbarfoo" :start 1 :end1 6 :start2 4 :end2 7)
;; end1 and end2 can be NIL or omitted to search to end of
;; string. except for string= and STRING-EQUAL, these functions return
;; the index in the first string where the mismatch occurred. if the
;; first string is a substring of the second, the return value is the
;; length of the substring.
