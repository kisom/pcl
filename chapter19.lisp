;;; code from chapter 19 of Practical Common Lisp, "Beyond Exception
;;; Handling: Conditions and Restarts"

;; conditions may represent any condition, not just errors, so the
;; lisp condition system is more general than typical exception
;; handling systems.

;; whereas most exception handling splits the code into two parts
;; (error signaling / throwing exceptions and error handling /
;; catching), Lisp splits it into three parts:
;; 1. condition signaling
;; 2. condition handling
;; 3. restarting

;; errors are not bugs: a missing file isn't necessarily the fault of
;; the program, and network connections being down almost always
;; aren't. however, failing to deal with the error is virtually always
;; a bug.

;; well-written programs are black boxes, and are built out of layers
;; of functions manifested in the call stack order. for example, if
;; #'high calls #'medium which then calls #'low, when control is in
;; #'low, it is also in #'medium and #'high.

;; because of this black box nature, function boundaries are a natural
;; place to deal with errors. the black box nature also means that if
;; a function in the call stack can do its job despite errors, the
;; higher functions do not need to know an error has
;; occurred. otherwise, the black box abstraction is broken.

;;; the lisp way
;; recovery code is separate from the code deciding how to recover;
;; recovery code can be placed in low-level functions.

;; example: application reading a textual log file
;; given a function #'PARSE-LOG-ENTRY that is passed a string
;; containing the a single log entry and returns a log-entry
;; object. this is called from parse-log-file that reads the log entry
;; and collects the log entries into a list.

;; parse-log-entry will not parse invalid log entries, but detects
;; when the entry is malformed and signals a condition.
