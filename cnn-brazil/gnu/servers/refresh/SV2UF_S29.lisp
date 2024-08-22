;; Advanced newLISPers often write a nameless function and supply it directly to the sort
;; command:

(sort (directory) (fn (a b) (< (length a) (length b))))

;; This does the same job, but saves 25 characters or so. You can use either fn or lambda to
;; deﬁne an inline or anonymous function.

;; 4.2.4 unique
;; unique returns a copy of a list with all duplicates removed:

(set 'data '( 1 1 2 2 2 2 2 2 2 3 2 4 4 4 4))
(unique data)
;-> (1 2 3 4)

;; There are also some useful functions for comparing lists. See Working with two or more
;; lists5 .

;; 4.2.5 sort
;; sort sorts a list in ascending order:

(sort '(4 2 1 3))
;-> (1 2 3 4)

;; sort can also sort a list in descending order by using the - sign:

(sort '(-4 -2 -1 -3))
;-> (-4 -3 -2 -1)

;; sort can also sort a list in descending order by providing a comparison function:

;; 4.2.5 ﬂat
;; ﬂat is useful for dealing with nested lists, because it shows what they look like without a
;; complicated hierarchical structure:

(set 'data '(0 (0 1 2) 1 (0 1) 0 1 (0 1 2) ((0 1) 0)))
(length data)
;-> 8
(length (flat data))
;-> 15
(flat data)
;-> (0 0 1 2 1 0 1 0 1 0 1 2 0 1 0)

;; 4.2.6 subseq
;; subseq returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing for the given length:


;; Fortunately, ﬂat is non-destructive, so you can use it without worrying about losing the
;; structure of nested lists:

data
;-> (0 (0 1 2) 1 (0 1) 0 1 (0 1 2) ((0 1) 0)) ; still nested


(subseq data 2 5)
;-> (1 2 1 0 1)

;; 4.2.7 reverse
;; reverse returns a new list that is a reversed copy of the original list:

(reverse '(1 2 3 4))
;-> (4 3 2 1)

;; 4.2.8 nreverse
;; nreverse returns a new list that is a reversed copy of the original list, with the given
;-> length:

(nreverse '(1 2 3 4) 2)
;-> (2 1 4 3)

;; 4.2.9 take
;; take returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing for the given length:

(take '(1 2 3 4) 2)
;-> (1 2)

;; 4.2.10 drop
;; drop returns a new list that is a copy of the original list, but with the given number
;; of elements removed from the beginning:

(drop '(1 2 3 4) 2)
;-> (3 4)

;; 4.2.11 split-at
;; split-at returns a tuple of two lists, where the first list contains the elements up to
;; the given index, and the second list contains the remaining elements:

(split-at 2 '(1 2 3 4))


;; 4.2.12 take-while
;; take-while returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing while the given function returns true for
;; each element:



;; 4.2.13 drop-while
;; drop-while returns a new list that is a copy of the original list, but with the
;; elements up to the given index removed while the given function returns true for
;; each element:

;; 4.2.14 partition
;; partition returns a tuple of two lists, where the first list contains the elements
;; for which the given function returns true, and the second list contains the
;; remaining elements:

;; 4.2.15 group
;; group returns a list of lists, where each inner list contains all the elements in the
;; original list that have the same value according to the given function:

;; 4.2.16 intersperse
;; intersperse returns a new list that is a copy of the original list, with the given
;; element interspersed between each pair of elements:

;; 4.2.17 cycle
;; cycle returns a new list that is an infinite repetition of the original list:

;; 4.2.18 split-with
;; split-with returns a tuple of two lists, where the first list contains the elements
;; up to the given index, and the second list contains the remaining elements, with
;; the given function applied to each element:

;; 4.2.19 split-at-most
;; split-at-most returns a tuple of two lists, where the first list contains the
;; elements up to the given index, and the second list contains the remaining elements,
;; with the given length:

;; 4.2.20 interleave
;; interleave returns a new list that is a copy of the original list, with the given
;; lists interleaved between each pair of elements:

;; 4.2.21 flatten
;; flatten returns a new list that is a copy of the original list, with all nested
;; lists flattened:

;; 4.2.22 interpose
;; interpose returns a new list that is a copy of the original list, with the given
;; element interspersed between each pair of elements:

;; 4.2.23 map
;; map applies the given function to each element of the original list and returns a new
;; list with the results:

;; 4.2.24 mapcat
;; mapcat applies the given function to each element of the original list and concatenates
;; the results into a new list:

;; 4.2.25 for-each
;; for-each applies the given function to each element of the original list, but does
;; not return a new list:

;; 4.2.26 reduce
;; reduce applies the given function to each pair of adjacent elements in the original
;; list, and returns a single value:

;; 4.2.27 filter
;; filter returns a new list that contains only the elements for which the given
;; function returns true:

;; 4.2.28 remove
;; remove returns a new list that contains only the elements for which the given
;; function returns false:

;; 4.2.29 some
;; some returns true if the given function returns true for at least one element in the
;; original list:

;; 4.2.30 every
;; every returns true if the given function returns true for all elements in the
;-> original list:

;; 4.2.31 find
;; find returns the first element for which the given function returns true:

;; 4.2.32 find-index
;; find-index returns the index of the first element for which the given function returns
;; true:

;; 4.2.33 sort
;; sort sorts a list in ascending order:

;; 4.2.34 unique
;; unique returns a copy of a list with all duplicates removed:

;; 4.2.35 sort
;; sort sorts a list in ascending order:

;; 4.2.36 flat
;; flat is useful for dealing with nested lists, because it shows what they look like
;; without a complicated hierarchical structure:

;; 4.2.37 subseq
;; subseq returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing for the given length:

;; 4.2.38 reverse
;; reverse returns a new list that is a reversed copy of the original list:

;; 4.2.39 nreverse
;; nreverse returns a new list that is a reversed copy of the original list, with the
;; given length:

;; 4.2.40 take
;; take returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing for the given length:

;; 4.2.41 drop
;; drop returns a new list that is a copy of the original list, but with the given number
;; of elements removed from the beginning:

;; 4.2.42 split-at
;; split-at returns a tuple of two lists, where the first list contains the elements up to
;; the given index, and the second list contains the remaining elements:

;; 4.2.43 take-while
;; take-while returns a new list that is a contiguous subsequence of the original list,
;; starting at the given index and continuing while the given function returns true for
;; each element:

;; 4.2.44 drop-while
;; drop-while returns a new list that is a copy of the original list, but with the
;; elements up to the given index removed while the given function returns true for
;; each element:

;; 4.2.45 partition
;; partition returns a tuple of two lists, where the first list contains the elements
;; for which the given function returns true, and the second list contains the
;; remaining elements:

;; 4.2.46 group
;; group returns a list of lists, where each inner list contains all the elements in the
;; original list that have the same value according to the given function:

;; 4.2.47 intersperse
;; intersperse returns a new list that is a copy of the original list, with the given
;; element interspersed between each pair of elements:

;; 4.2.48 cycle
;; cycle returns a new list that is an infinite repetition of the original list:

;; 4.2.49 split-with
;; split-with returns a tuple of two lists, where the first list contains the elements
;; up to the given index, and the second list contains the remaining elements, with
;-> the given function applied to each element:

;; 4.2.50 split-at-most
;; split-at-most returns a tuple of two lists, where the first list contains the
;; elements up to the given index, and the second list contains the remaining elements,
;; with the given length:

;; 4.2.51 interleave
;; interleave returns a new list that is a copy of the original list, with the given
;; lists interleaved between each pair of elements:

;; 4.2.52 flatten
;; flatten returns a new list that is a copy of the original list, with all nested
;; lists flattened:

;; 4.2.53 interpose
;; interpose returns a new list that is a copy of the original list, with the given
;; element interspersed between each pair of elements:

;; 4.2.54 map
;; map applies the given function to each element of the original list and returns a new
;; list with the results:

;; 4.2.55 mapcat
;; mapcat applies the given function to each element of the original list and concatenates
;; the results into a new list:

;; 4.2.56 for-each
;; for-each applies the given function to each element of the original list, but does
;; not return a new list:

;; 4.2.57 reduce
;; reduce applies the given function to each pair of adjacent elements in the original
;; list, and returns a single value:

;; 4.2.58 filter
;; filter returns a new list that contains only the elements for which the given
;; function returns true:

;; 4.2.59 remove
;; remove returns a new list that contains only the elements for which the given
;; function returns false:

;; 4.2.60 some
;; some returns true if the given function returns true for at least one element in the
;; original list:

;; 4.2.61 every
;; every returns true if the given function returns true for all elements in the
;; original list:

;; 4.2.62 find
;; find returns the first element for which the given function returns true:

;; 4.2.63 find-index
;; find-index returns the index of the first element for which the given function returns
;; true:

;; 4.2.64 sort
;; sort sorts a list in ascending order:

;; 4.2.65 unique
;; unique returns a copy of a list with all duplicates removed:

;; 4.2.66 sort
;; sort sorts a list in ascending order:

;; 4.2.67 flat
;; flat is useful for dealing with nested lists, because it shows what they look like
;; without a complicated hierarchical structure:

;; 4.2.6 transpose

;; transpose is designed to work with matrices (a special type of list: see Matrices6 ). It also
;; does something useful with ordinary nested lists. If you imagine a list of lists as a table, it
;; ﬂips rows and columns for you:

(set 'a-list
     '(("a" 1)
      ("b" 2)
      ("c" 3)))
(transpose a-list)
;-> (("a" "b" "c")
;; ( 1 2 3))

(set 'table
'((A1 B1 C1 D1 E1 F1 G1 H1)
(A2 B2 C2 D2 E2 F2 G2 H2)
(A3 B3 C3 D3 E3 F3 G3 H3)))


(transpose table)


;-> ((A1 A2 A3)
;; (B1 B2 B3)
;; (C1 C2 C3)
;; (D1 D2 D3)
;; (E1 E2 E3)

;; And here's a nice bit of newLISP sleight of hand:

(set 'table '((A 1) (B 2) (C 3) (D 4) (E 5)))
;-> ((A 1) (B 2) (C 3) (D 4) (E 5))
(set 'table (transpose (rotate (transpose table))))
;-> ((1 A) (2 B) (3 C) (4 D) (5 E))


;; 4.3 Vectors
;; Vectors are a very simple and powerful data structure in Lisp. They are like lists,
;; except that they are immutable. This means that once a vector is created, you can't change
;; its elements.

;; 4.3.1 Creating a Vector
;; To create a vector, you use the vector function, which takes a list of elements as an
;; argument:

(vector 1 2 3 4 5)
;-> #(1 2 3 4 5)

;; 4.3.2 Accessing Elements
;; You can access elements of a vector using the vector-ref function, which takes two
;; arguments: the vector and the index:

(vector 1 2 3 4 5)

;; 4.2.7 explode
;; The explode function lets you blow up a list:


;; 4.3.8 Converting Lists to Vectors
;; You can convert a list to a vector using the vector function:

(list 1 2 3 4 5)
;-> (1 2 3 4 5)


;; 4.3.9 Converting Vectors to Lists
;; You can convert a vector to a list using the list function:

(vector 1 2 3 4 5)
;-> #(1 2 3 4 5)

;; 4.3.10 Vector Length
;; You can get the length of a vector using the vector-length function:

(vector 1 2 3 4 5)
;-> 5

;; 4.3.11 Vector Concatenation
;; You can concatenate vectors using the vector-append function:

(vector 1 2 3)
;-> #(1 2 3)
(vector 4 5 6)
;-> #(4 5 6)
(vector-append (vector 1 2 3) (vector 4 5
6))
;-> #(1 2 3 4 5 6)

;; 4.3.12 Vector Repetition
;; You can repeat a vector using the vector-repeat function:

(vector 1 2 3)
;-> #(1 2 3)
(vector-repeat (vector 1 2 3) 3)
;-> #(1 2 3 1 2 3 1 2 3

;; 4.3.13 Vector Indexing
;; You can index a vector using the vector-ref function:

(vector 1 2 3 4 5)
;-> 3

;; 4.3.14 Vector Slicing
;; You can slice a vector using the vector-slice function:

(vector 1 2 3 4 5)
;-> #(2 3 4)

;; 4.3.15 Vector Mutation
;; Vectors are immutable, so you can't change their elements directly. If you want to
;; change an element of a vector, you have to create a new vector with the desired
;; changes:

(vector 1 2 3 4 5)
;-> #(1 2 3 4 5)


;; 4.4 Strings
;; Strings are a sequence of characters in Lisp. You can create a string using the
;; string function, which takes a list of characters as an argument:

(string #\a #\b #\c #\d #\e)
;-> "abcde"

;; 4.4.1 Accessing Characters
;; You can access characters in a string using the string-ref function, which takes two
;; arguments: the string and the index:

(string #\a #\b #\c #\d #\e)
;-> #\a

;; 4.4.2 String Length
;; You can get the length of a string using the string-length function:

(string #\a #\b #\c #\d #\e)
;-> 5

;; 4.4.3 String Concatenation
;; You can concatenate strings using the string-append function:

(string "abc" "def")
;-> "abcdef"

;; 4.4.4 String Repetition
;; You can repeat a string using the string-repeat function:

(string "abc" 3)
;-> "abcabcabc"

;; 4.4.5 String Indexing
;; You can index a string using the string-ref function:

(string #\a #\b #\c #\d #\e)
;-> #\a

;; 4.4.6 String Slicing
;; You can slice a string using the string-slice function:

(string #\a #\b #\c #\d #\e)
;-> "bc"

;; 4.4.7 String Mutation
;; Strings are immutable, so you can't change their characters directly. If you want to
;; change a character of a string, you have to create a new string with the desired
;; changes:

(string #\a #\b #\c #\d #\e)
;-> "abcde"

;; 4.5 Pairs and Lists
;; Pairs and lists are the fundamental building blocks of Lisp. A pair is a simple
;; data structure consisting of two elements, a car and a cdr. A list is a sequence
;; of elements, where each element is either a pair or a simple value.
;; 4.5.1 Creating Pairs
;; To create a pair, you use the cons function, which takes two arguments: the car and
;; the cdr:

(cons 1 2)
;-> (1. 2)

;; 4.5.2 Accessing Elements
;; You can access the car and cdr of a pair using the car and cdr functions,
;; respectively:

(cons 1 2)
;-> 1
(car (cons 1 2))
;-> 1

;; 4.5.3 Creating Lists
;; To create a list, you use the list function, which takes zero or more arguments:

(list 1 2 3 4 5)
;-> (1 2 3 4 5)

;; 4.5.4 Accessing Elements
;; You can access elements of a list using the list-ref function, which takes two
;; arguments: the list and the index:

(list 1 2 3 4 5)
;-> 1

;; 4.5.5 List Length
;; You can get the length of a list using the length function:

(list 1 2 3 4 5)
;-> 5

;; 4.5.6 List Concatenation
;; You can concatenate lists using the append function:

(list 1 2 3)
;-> (1 2 3)
(list 4 5 6)
;-> (4 5 6)
(append (list 1 2 3) (list 4 5 6))
;-> (1 2 3 4 5 6)

;; 4.5.7 List Repetition
;; You can repeat a list using the repeat function:

(list 1 2 3)
;-> (1 2 3)
(repeat (list 1 2 3) 3)
;-> ((1 2 3) (1 2 3) (1 2
3))

;; 4.5.8 List Indexing
;; You can index a list using the list-ref function:

(list 1 2 3 4 5)
;-> 3

;; 4.5.9 List Slicing
;; You can slice a list using the slice function:

(list 1 2 3 4 5)
;-> (2 3 4)

;; 4.5.10 List Mutation
;; Lists are mutable, so you can change their elements directly:

(list 1 2 3 4 5)
;-> (1 2 3 4 5)

;; 4.6 Symbolics
;; Symbols are identifiers in Lisp, which are used to refer to values or functions.
;; 4.6.1 Creating Symbols
;; To create a symbol, you use the symbol function:

(symbol "hello")
;-> hello

;; 4.6.2 Accessing Symbols
;; You can access the name of a symbol using the symbol-name function:

(symbol "hello")
;-> "hello"

;; 4.6.3 Symbol Mutation
;; Symbols are immutable, so you can't change their name directly. If you want to
;; change a symbol's name, you have to create a new symbol with the desired
;; changes:

(symbol "hello")
;-> hello

;; 4.7 Functions
;; Functions are the building blocks of Lisp code. A function takes zero or more
;; arguments and returns a value. Functions can be defined using the lambda
;; form, which takes zero or more arguments and a body:

(lambda (x y) (+ x y))
;-> #<CLOSURE (LAMBDA (X Y) (+ X Y)) {10
               ...}>

;; 4.7.1 Calling Functions
;; You can call a function by evaluating it with the appropriate arguments:

((lambda (x y) (+ x y)) 3 4)
;-> 7

;; 4.7.2 Function Arguments
;; Functions can have any number of arguments, and they can have any number of
;; different types of arguments. You can pass arguments of different types to a
;; function:


(lambda (x y) (+ x y))
;-> 7
(lambda (x y z) (+ x y z))
;-> 12

;; 4.7.3 Function Return Values
;; Functions can return values using the return form:

(lambda (x)
  (if (> x 0)
      x
      (- x)))


(lambda (x)
  (if (> x 0)
      x
      (- x)))


;; 4.7.4 Function Definitions
;; Functions can be defined using the define form, which takes a symbol as the
;; name of the function and a body:

(define (square x)
  (* x x))

;; 4.7.5 Function Anonymous
;; Functions can also be defined using the lambda form without a symbol, which is
;; called an anonymous function:


(lambda (x) (* x x))

;; 4.8 Macros
;; Macros are special forms that allow you to define new syntax for existing
;; functions. Macros can be used to generate new functions, manipulate code, and
;; manipulate data structures.
;; 4.8.1 Defining Macros
;; To define a macro, you use the macro form, which takes a symbol as the
;; name of the macro and a body:

(defmacro repeat (count expr)
  `(loop (decf count 1)
     (if (<= count 0)
     expr)
     expr))
      (repeat count expr)))
      (repeat 3 (list 1 2 3)))
      ;-> (1 2 3 1 2 3 1 2 3


;; 4.8.2 Calling Macros
;; You can call a macro by evaluating it with the appropriate arguments:

((repeat 3 (list 1 2 3))
;-> (1 2 3 1 2 3 1 2 3



;; 4.9 Special Forms
;; Special forms are built-in forms that have special behavior in Lisp. Some
;; special forms include quote, unquote, unquote-splicing, and lambda.
;; 4.9.1 Quote
;; The quote special form is used to quote a value without evaluating it:

(quote (1 2 3))
;-> (1 2 3)


;; 4.9.2 Unquote
;; The unquote special form is used to evaluate a quoted value:

(list 1 (unquote 2) 3)
;-> (1 2 3)

;; 4.9.3 Unquote-Splicing
;; The unquote-splicing special form is used to evaluate a quoted list and
;; splice the evaluated list into the current expression:

(list 1 @'(2 3) 4)
;-> (1 2 3 4)


;; 4.9.4 Lambda
;; The lambda special form is used to create a new function:

((lambda (x y) (+ x y)) 3 4)
;-> 7

;; 4.10 Conclusion
;; Congratulations! You have completed the Lisp tutorial. I hope you found the
;; content helpful and enjoyable. If you have any further questions, please don't
;; hesitate to ask.


;; 4.2.7 explode
;; The explode function lets you blow up a list:

(explode (sequence 1 10))
;-> ((1) (2) (3) (4) (5) (6) (7) (8) (9) (10))
;; You can specify the size of the pieces, too:
(explode (sequence 1 10) 2)
;-> ((1 2) (3 4) (5 6) (7 8) (9 10))
(explode (sequence 1 10) 3)
;-> ((1 2 3) (4 5 6) (7 8 9) (10))
(explode (sequence 1 10) 4)
;-> ((1 2 3 4) (5 6 7 8) (9 10))

;; 4.3 List analysis: testing and searching

;; Often you don't know what's in a list, and you want some forensic tools to ﬁnd out more
;; about what's inside it. newLISP has a good selection.
;; We've already met length, which ﬁnds the number of elements in a list.
;; The starts-with and ends-with functions test the start and ends of lists:


(starts-with '(1 2 3) '(1 2))
(starts-with alphabet "a")
;-> true; list starts with item "a"?
(starts-with (join alphabet) "abc")
;-> true; convert to string and test
(ends-with alphabet "z")
;-> true



;; 4.3.1 find
;; The find function searches for a value in a list:

(find 3 '(1 2 3 4 5))
;; 4.3.2 member
;; The member function tests whether a value is in a list:

(member 3 '(1 2 3 4 5))
;; 4.3.3 count
;; The count function counts the number of occurrences of a value in a list:

(count 3 '(1 2 3 3 4 5))

;; 4.3.4 remove
;; The remove function removes a value from a list:

(remove 3 '(1 2 3 4 3 5))


;; 4.3.5 remove-duplicates

;; These functions work equally well with strings, too (and they take regular expressions). See
;; Testing and comparing strings7 .
;; How about contains? In fact there isn't one single newLISP function that does this job.
;; Instead you have ﬁnd, match, member, ref, ﬁlter, index, and count, among others.
;; Which one you use depends on what sort of answer you want to the question Does this
;; list contain this item?, and whether the list is a nested list or a ﬂat one.
;; If you want a simple answer with a quick top-level-only search, use ﬁnd. See ﬁnd8 .
;; If you want the item and the rest of the list as well, use member. See member9 .
;; If you want the index number of the ﬁrst occurrence, even if the list contains nested lists,
;; you can use ref. See ref and ref-all10 .
;; If you want a new list containing all the elements that match your search element, use
;; ﬁnd-all. See ﬁnd-all11 .
;; If you want to know whether or not the list contains a certain pattern of elements, use
;; match. See matching patterns in lists12 .
;; You can ﬁnd all list items that satisfy a function, either built-in or one of your own, with
;; the ﬁlter, clean, and index functions. See Filtering lists: ﬁlter, clean, and index13 .
;; The exists and for-all functions check elements in a list to see if they pass a test.
;; If you want to ﬁnd elements in a list purely to change them to something else, then don't
;; bother to look for them ﬁrst, just use replace. See Replacing information: replace14 . You
;; can also ﬁnd and replace list elements using the set-ref function. See Find and replace
;; matching elements15 .
;; If you want to know how many occurrences there are of items in the list, use count. See
;; Working with two or more lists16 .
;; Let's look at some examples of each of these.


;; 4.3.1 ﬁnd

;; ﬁnd looks through a list for an expression and returns an integer or nil. The integer is the
;; index of the ﬁrst occurrence of the search term in the list. It's possible for ﬁnd to return
;; 0 - if the list starts with the item, its index number is 0, but this isn't a problem - you can
;; use this function in an if test, because 0 evaluates to true.

(set 'sign-of-four
     (parse (read-file "/Users/me/Sherlock-Holmes/sign-of-four.txt")
     {\W} 0))

(if (find "Moriarty" sign-of-four)
    (println "Moriarty is mentioned")
    (println "No mention of Moriarty"))
; Moriarty anywhere?

;; No mention of Moriarty
(if (find "Lestrade" sign-of-four)
    (println "Lestrade is mentioned")
    (println "No mention of Lestrade"))

;; Lestrade is mentioned.
(find "Watson" sign-of-four)
;-> 477



;; 4.3.2 member

;; member tests whether a value is in a list. It returns a boolean value.
(member 3 '(1 2 3 4 5))
;-> true
(member 3 '(1 2 3 4))
;-> false

;; 4.3.3 count

;; count counts the number of occurrences of a value in a list.
(count 3 '(1 2 3 3 4 5))
;;-> 2

;; 4.3.4 remove
;; remove removes a value from a list. It returns a new list.

(remove 3 '(1 2 3 4 3 5))
;;-> (1 2 4 5)

;; 4.3.5 remove-duplicates
;; remove-duplicates removes duplicate elements from a list. It returns a new list.

(remove-duplicates '(1 2 3 3 4 5))

;;-> (1 2 3 4 5)

;; 4.4.6 nth
;; nth returns the nth element of a list.

(nth 2 '(1 2 3 4 5))
;;-> 3

;; 4.4.7 reverse
;; reverse returns a new list with the elements of the original list in reverse order.

(reverse '(1 2 3 4 5))

;;-> (5 4 3 2 1)
;; 4.4.8 sort
;; sort returns a new list with the elements of the original list in ascending order.

(sort '(5 3 2 1 4))
;;-> (1 2 3 4 5)

;; 4.4.9 join
;; join returns a string with the elements of the original list separated by a given string.

(join " " '(1 2 3 4 5))
;;-> "1 2 3 4 5"

;; 4.4.10 map
;; map applies a function to each element of a list and returns a new list.

(map (lambda (x) (* x 2)) '(1 2 3 4))
;;-> (2 4 6 8)


;; 4.4.11 filter
;; filter returns a new list with only the elements of the original list for which a given function
;; returns true.

(filter (lambda (x) (> x 3)) '(1 2 3 4))
;;-> (4)

;; 4.4.12 reduce
;; reduce applies a function to the elements of a list, combining them in a way that results in
;; a single value.

(reduce (lambda (acc x) (+ acc x)) '(1 2 3 4))
;;-> 10

;; 4.4.13 concat
;; concat returns a new list with the elements of two or more lists.

(concat '(1 2 3) '(4 5 6))


;; 4.4.14 remove-duplicates-from-list
;; remove-duplicates-from-list returns a new list with duplicate elements removed from the original list
(remove-duplicates-from-list '(1 2 3 3 4 5))


;; 4.4.15 deep-copy-list
;; deep-copy-list returns a new list with a deep copy of the original list.
(define original '(1 2 (3 4) 5))
(define copied (deep-copy-list original))
(set-car copied (10))
(print original)
(print copied)
;;-> (1 2 (3 4) 5)



;; 4.4.16 length
;; length returns the number of elements in a list.
(length '(1 2 3 4 5))
;;-> 5

;; 4.4.17 append
;; append returns a new list with the elements of two or more lists.

(append '(1 2 3) '(4 5 6))
;;-> (1 2 3 4 5 6)

;; 4.4.18 car
;; car returns the ﬁrst element of a list.

(car '(1 2 3 4 5))

;;-> 1
;; 4.4.19 cdr
;; cdr returns a new list with all but the ﬁrst element of the original

(cdr '(1 2 3 4 5))
;;-> (2 3 4 5)

;; 4.4.20 set-car!
;; set-car! changes the ﬁrst element of a list.

(set-car! '(1 2 3 4 5) 10)
(print '(1 2 3 4 5))

;;-> (10 2 3 4 5)

;; 4.4.21 set-cdr!
;; set-cdr! changes the rest of a list.

(set-cdr! '(1 2 3 4 5) '(6 7))
;;-> (1 2 3 6 7 5)

;; 4.4.22 reverse-in-place
;; reverse-in-place reverses the order of elements in a list.

(reverse-in-place '(1 2 3 4 5))



;; 4.4.23 sort-in-place
;; sort-in-place sorts the elements of a list in ascending order.

(sort-in-place '(5 3 2 1 4))
;;-> (1 2 3 4 5)

;; 4.4.24 insert-at-index
;; insert-at-index inserts an element at a given index in a list.
(insert-at-index '(1 2 3 4 5) 3
   '(6 7 8))
   ;;-> (1 2 3 6 7 8 4 5)
   
;; 4.4.25 remove-at-index
;; remove-at-index removes an element at a given index from a list.

(remove-at-index '(1 2 3 4 5) 3)



;; 4.4.26 find-index
;; find-index finds the index of the ﬁrst occurrence of a value in a
;; list.

(find-index 3 '(1 2 3 4 5))


;; 4.4.27 remove-duplicates-from-list
;; remove-duplicates-from-list removes duplicate elements from a list.

(remove-duplicates-from-list '(1 2 3 3 4 5))


;; 4.4.28 deep-copy-list
;; deep-copy-list returns a new list with a deep copy of the original list.

(define original '(1 2 (3 4) 5))
(define copied (deep-copy-list original))
(set-car copied (10))
(print original)
(print copied)

;; Here I've parsed the text of Sir Arthur Conan Doyle's The Sign of Four (which you can
;; download from Project Gutenberg), and tested whether the resulting list of strings contains
;; various names. The integer returned is the ﬁrst occurrence of the string element in the list.
;; ﬁnd lets you use regular expressions, too, so you can ﬁnd any string elements in a list that
;; match a string pattern:

(set 'loc (find "(cookies|cokepaste|chocolate|pizza)" sign-of-four 0))
     (if loc
     (println "The snacks " (sign-of-four loc) " is true.")
     (println "No trace of snacks"))



;; 4.4.29 length
;; length returns the number of elements in a list.
(length '(1 2 3 4 5))
;;-> 5

;; 4.4.30 append
;; append returns a new list with the elements of two or more lists.
(append '(1 2 3) '(4 5 6))
;;-> (1 2 3 4 5 6)







