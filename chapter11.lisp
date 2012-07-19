;;; code from chapter 11 of Practical Common Lisp, "Collections"
;;; http://www.gigamonkeys.com/book/collections.html

;; collections: data types collect multiple values into a single
;; object

;; vectors and lists share enough characteristics that they are
;; treated as subtypes of the sequence general abstraction

;;; vectors
;; basic integer-indexed collection
;; fixed-size and resizeable

;; fixed size vectors can be created with VECTOR, which takes any
;; number of arguments and converts them into a vector. vector literal
;; notation is #(), which allows PRINT and READ persistence. the
;; effects of modifying literals isn't defined, so vectors that will
;; be modified should be created with VECTOR or MAKE-ARRAY.

;; MAKE-ARRAY can be used to create vectors of arbitrary
;; dimensionality; it's only required argument is a list of
;; dimensions. if given a single number, it will create a one
;; dimensional vector. it accepts an :initial-element value to set
;; each element to.
(print (make-array '(5 5) :initial-element nil))

;; MAKE-ARRAY is also used to create resizeable vectors. resizeable
;; vectors make use of fill pointers, the index of the next position
;; to be filled when an element is added. use the :fill-pointer
;; argument to MAKE-ARRAY:
(print (make-array 5 :fill-pointer 0))

;; this creates a vector with room for five elements but looks empty
;; because the fill pointer is zero.

;; you can add an element to the end with VECTOR-PUSH; VECTOR-POP
;; removes an element from the end. this vector currently has room for
;; only five elements. if any other elements are pushed, VECTOR-PUSH
;; will return NIL and the array remains untouched. the :adjustable
;; keyword argument must be passed:
(make-array 5 :fill-pointer 0 :adjustable t)

;; now, VECTOR-PUSH-EXTEND is identical to VECTOR-PUSH except that it
;; will expand the vector as required.

;;; subtypes of vector
;; specialised vectors are restricted to holding certain types of
;; elements; they may provide performance benefits as the compiler
;; doesn't have to guess or generalise for the contents.

;; for example, strings are character vectors; string literals are
;; fixed-sized character vectors. resizeable-strings can be created
;; using the MAKE-ARRAY function, specifying an :element-type of CHARACTER:
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)

;; bit vectors use the element-type BIT.

;;; vectors as sequences
;; two most basic sequence functions: LENGTH and ELT
;; LENGTH treats vectors with fill pointers as having a length equal
;; to the index of of the fill pointer.
;; ELT is for accessing elements: (ELT sequence index), for example
(defvar *my-array-1* (vector 1 2 3 4 5 6 7 8 9))
(length *my-array-1*)          ;; 9
(elt *my-array-1* 0)           ;; 1
(elt *my-array-1* 2)           ;; 3

;; ELT treats a vector with a fill pointer as having that length; it
;; will signal an error on an out-of-bounds access. ELT is also a
;; STFable place.

;;; sequence iterating functions
;; Table 11-1.Basic Sequence Functions

;; Name       Required Arguments          Returns
;; COUNT      Item and sequence           Number of times item appears
;;                                        in sequence
;; FIND       Item and sequence           Item or NIL
;; POSITION   Item and sequence           Index into sequence or NIL
;; REMOVE     Item and sequence           Sequence with instances of
;;                                        item removed
;; SUBSTITUTE New item, item, and         Sequence with instances of
;;            sequence                    item replaced with new
;;                                        item

;; note REMOVE and SUBSTITUTE return sequence of the same type

;; keyword arguments:
;; :test      function returning a        used to compare item to each
;;            bool, takes two args        item
;; :key       function taking one arg     used to extract a key value
;;                                        which is compared in place
;;                                        of the item itself (elements
;;                                        such as FIND return the
;;                                        actual element)
;; :start and :end
;; :from-end  boolean                     if not nil, sequence is
;;                                        searched in reverse order;
;;                                        by itself, only effects FIND
;;                                        and POSITION
;; :count     integer                     specifies how many elements
;;                                        to remove or substitute; can
;;                                        be used in conjunction with
;;                                        REMOVE and SUBSTITUTE
;; by itself, :from-end doesn't effect COUNT, but :key and :test can
;; affect the results.
;; :count

;;; higher-order function variants
;; variants of the previous with -IF appended take a function argument
;; in place of the item argument; only elements for which the function
;; returns true are returned; with -IF-NOT, if function returns NIL

;; note that -IF-NOT is technically deprecated but considered
;; ill-advised. for example, REMOVE-IF-NOT returns items that match
;; the predicate.

;; these functions take the same keyword args except :test

;; there is also REMOVE-DUPLICATES, which takes only a sequence and
;; removes any duplicates.
