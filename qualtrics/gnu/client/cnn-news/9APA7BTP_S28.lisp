;; 4.1.2 push: pushing items onto lists

;; push is a powerful command that you can use to make a new list or insert an element at
;; any location in an existing list. Pushing an element at the front of the list moves everything
;; one place to the right, whereas pushing an element at the end of the list just attaches it
;; and makes a new last element. You can also insert an element anywhere in the middle of a
;; list.

;; Despite its constructive nature, it's technically a destructive function, because it changes
;; the target list permanently, so use it carefully. It returns the value of the element that was
;; inserted, rather than the whole list.


;; Example:
;; (push 1 '(2 3 4)) => '(1 2 3)
;; (push 'a '(b c d)) => '(a b c d)
;; (push 'x '(y z)) => '(x y z)

(defun push (item list)
  (cons item list))

;; 4.1.3 pop: removing items from lists


;; pop is another powerful command that you can use to remove an element from a list.


;; Example:
;; (pop '(1 2 3)) => '(1 2)
;; (pop '(a b c d)) => '(a b c)
;; (pop '()) => "List is empty"

(defun pop (list)
  (if (null list)
      "List is empty"
      (cdr list)))

;; 4.1.4 reverse: reversing lists

;; When you refer to the location of an element in a list, you use zero-based numbering, which
;; you would expect if you're an experienced programmer:



;; Example:
;; (reverse '(1 2 3 4 5)) => '(5 4
;; (reverse '(a b c d e)) => '(e d c b a)

(defun reverse (list)
  ;; Base case: an empty list is already reversed
  (if (null list)
      list
      ;; Recursive case: reverse the rest of the list and prepend the first element
      (reverse (cdr list))
      (cons (car list) (reverse (cdr list)))))

 ;; 4.1.5 length: calculating the length of a list
 ;; The length of a list is the number of elements it contains.
 ;; The length of an empty list is 0.
 ;; The length of a non-empty list is 1 plus the length of the rest of the
 ;; list.

 ;; Example:
 ;; (length '(1 2 3 4 5)) => 5
 ;; (length '(a b c d e)) => 5
 ;; (length '()) => 0
 (defun length (list)
  ;; Base case: an empty list has a length of 0
  (if (null list)
      0
      ;; Recursive case: add 1 to the length of the rest of the list
      (1+ (length (cdr list)))))

 ;; 4.1.6 append: combining lists
 ;; The append function combines two lists into a single list.

 ;; Example:
 ;; (append '(1 2 3) '(4 5 6)) => '(1
 ;; (append '(a b c) '(d e f)) => '(a b c d e
 ;; (append '() '(a b c)) => '(a b c)
 (defun append (list1 list2)
  ;; Base case: if either list is empty, return the other list
  (if (null list1)
      list2
      ;; Recursive case: prepend the first element of the first list to the rest of the first list
      ;; and then append the rest of the first list to the rest of the second list
      (cons (car list1) (append (cdr list1) list2))))
      ;; 4.1.7 map: applying a function to each element in a list
      ;; The map function applies a function to each element in a list and returns a new list
      ;; containing the results.
      ;; The map function is a higher-order function, meaning it takes a function as an argument
      ;; and returns a new function.
      ;;
      ;; Example:
      ;; (map 'list '(1 2 3 4 5)) => '(2
      ;; (map 'list '(a b c d e)) => '(b c d e)
      ;; (map 'list '()) => '()
      (defun map (func list)
       ;; Base case: if the list is empty, return an empty list
       (if (null list)
           '()
           ;; Recursive case: apply the function to the first element of the list,
           ;; and then prepend the result to the rest of the map of the rest of the list
           (cons (func (car list)) (map func (cdr list)))))

 ;; 4.1.8 filter: creating a new list with only the elements that satisfy a
 ;; condition
 ;; The filter function creates a new list with only the elements that satisfy a given condition.
 ;; The filter function is a higher-order function, meaning it takes a function as an argument
 ;; and returns a new function.
 ;;
 ;; Example:
 ;; (filter 'even '(1 2 3 4 5)) => '(2
 ;; (filter 'even '(a b c d e)) => '(e)
 ;; (filter 'even '()) => '()
 (defun filter (predicate list)
  ;; Base case: if the list is empty, return an empty list
  (if (null list)
      '()
      ;; Recursive case: if the first element of the list satisfies the condition,
      ;; prepend it to the rest of the filter of the rest of the list
      ;; otherwise, skip it and continue filtering the rest of the list
      (if (predicate (car list))
       (cons (car list) (filter predicate (cdr list)))
       (filter predicate (cdr list)))))
       ;; 4.1.9 reduce: combining all elements in a list into a single value
       ;; The reduce function combines all elements in a list into a single value using a given function.
       ;; The reduce function is a higher-order function, meaning it takes a function as an argument
       ;; and returns a new function.
       ;;
       ;; Example:
       ;; (reduce 'add '(1 2 3 4 5)) => 15
       ;; (reduce 'add '(a b c d e)) => "Invalid input: list contains non
       ;; numbers"
       ;; (reduce 'add '()) => 0
       (defun reduce (func list)
        ;; Base case: if the list is empty, return 0
        (if (null list)
            0
            ;; Recursive case: apply the function to the first element of the list,
            ;; and then combine the result with the rest of the reduce of the rest of the list
            (+ (func (car list)) (reduce func (cdr list)))))

 ;; 4.1.10 sort: sorting a list in ascending order
 ;; The sort function sorts a list in ascending order using the built-in sort function.
 ;; The sort function is a higher-order function, meaning it takes a function as an argument
 ;; and returns a new function.
 ;;
 ;; Example:
 ;; (sort '(5 3 1 4 2)) => '(1 2
 ;; (sort '(a c b d e)) => '(a b c d e)
 ;; (sort '()) => '()
 (defun sort (list)
  ;; Base case: if the list has only one element, it's already sorted
  (if (<= (length list) 1)
      list
      ;; Recursive case: sort the rest of the list and then combine the first element
      ;; with the rest of the sorted list
      (cons (car (sort (filter (lambda (x) (> x (car list))) (
                 (cdr list)))))
          (sort (filter (lambda (x) (<= x (car list))) (
                 (cdr list)))))))))

 ;; 4.1.11 foldl: combining all elements in a list into a single
 ;; value using a left-associative binary function
 ;; The foldl function combines all elements in a list into a single value using a left-
 ;; associative binary function. The foldl function is a higher-order function, meaning
 ;; it takes a function as an argument and returns a new function.

 ;; Example:
 ;; (foldl 'add '(1 2 3 4 5)) => 1
 ;; (foldl 'add '(a b c d e)) => "Invalid input: list contains
 ;; non-numbers"
 ;; (foldl 'add '()) => 0
 (defun foldl (func initial list)
  ;; Base case: if the list is empty, return the initial value
  (if (null list)
      initial
      ;; Recursive case: apply the function to the first element of the list,
      ;; and then combine the result with the rest of the foldl of the rest of the list
      (func (car list) (foldl func initial (cdr list)))))

 ;; 4.1.12 foldr: combining all elements in a list into a single
 ;; value using a right-associative binary function
 ;; The foldr function combines all elements in a list into a single value using a right-
 ;; associative binary function. The foldr function is a higher-order function, meaning
 ;; it takes a function as an argument and returns a new function.
 ;;
 ;; Example:

;; If you don't specify a location or index, push pushes the new element at the front. Use
;; a third expression to specify the location or index for the new element. -1 means the last
;; element of the list, 1 means the second element of the list counting from the front from 0,
;; and so on:

(set 'vowels '("a" "e" "i" "o"))
(push "u" vowels -1)
;-> "u"
; vowels is now ("a" "e" "i" "o" "u")
(set 'evens '(2 6 10))    ; goes before the 10
(push 8 evens -2)         ; goes after the 2
(push 4 evens 1)          ; evens is now (2 4 6 8 10)

 ;; 4.1.13 zip: combining two lists into a list of pairs
 ;; The zip function combines two lists into a list of pairs. The first element of the first
 ;; list is paired with the first element of the second list, the second element of the first
 ;; list is paired with the second element of the second list, and so on.

 ;; Example:
 ;; (zip '(1 2 3) '(a b c)) => ((1 a)
 ;; (zip '(a b c) '(d e f)) => ((a d) (b
 ;; (zip '() '(a b c)) => '())
 (defun zip (list1 list2)
  ;; Base case: if either list is empty, return an empty list
  (if (or (null list1) (null list2))
      '()
      ;; Recursive case: combine the first elements of the two lists into a pair,
      ;; and then zip the rest of the first list with the rest of the second list
      (cons (cons (car list1) (car list2)) (zip (cdr list1)
       (cdr list2)))))

 ;; 4.1.14 unzip: splitting a list of pairs into two lists
 ;; The unzip function splits a list of pairs into two lists. The first element of each pair
 ;; is paired with the first element of the resulting first list, the second element of each
 ;; pair is paired with the first element of the resulting second list, and so on.
 ;;
 ;; Example:
 ;; (unzip '((1 a) (2 b) (3 c))) => ((1
 ;; (unzip '((a d) (b e) (c f))) => ((a
 ;; (unzip '()) '()) => (()))
 (defun unzip (pairs)
  ;; Base case: if the list of pairs is empty, return two empty lists
  (if (null pairs)
      (values '() '())
      ;; Recursive case: split the first pair into two elements,
      ;; and then recursively unzip the rest of the pairs
      (let ((first (car pairs))
       (rest (cdr pairs)))
       (values (car first) (car (car rest)))
        (unzip (cdr rest)))))

 ;; 4.1.15 intersperse: inserting a separator between elements in a list
 ;; The intersperse function inserts a separator between elements in a list.

 ;; Example:
 ;; (intersperse 'x '(1 2 3 4 5)) => (
 ;; (intersperse 'x '(a b c d e)) => (a b x c
 ;; (intersperse 'x '()) => '())
 (defun intersperse (separator list)
  ;; Base case: if the list has only one element, return the list unchanged
  (if (<= (length list) 1)
      list
      ;; Recursive case: insert the separator between the first element and the rest of the
      ;; intersperse of the rest of the list
      (cons (car list) (intersperse separator (cdr list)))))

;; If the symbol you supply as a list doesn't exist, push usefully creates it for you, so you
;; don't have to declare it ﬁrst.

(for (c 1 10)
(push c number-list -1)
(println number-list))


;; 4.1.16 reverse: reversing a list
;; The reverse function reverses a list.

 ;; Example:
 ;; (reverse '(1 2 3 4 5)) => (5 4
 ;; (reverse '(a b c d e)) => (e d c b a)
 ;; (reverse '()) => '()
 (defun reverse (list)
  ;; Base case: if the list has only one element, return the list unchanged
  (if (<= (length list) 1)
      list
      ;; Recursive case: reverse the rest of the list and then prepend the first element
      (cons (car list) (reverse (cdr list)))))

;; By the way, there are plenty of other ways of generating a list of unsorted numbers. You
;; could also do a number of random swaps like this:

(set 'l (sequence 0 99))
    (dotimes (n 100)
        (swap (l (rand 100)) (l (rand 100)))))



;; 4.1.17 nconc: combining two lists into a single list
;; The nconc function combines two lists into a single list.

 ;; Example:
 ;; (nconc '(1 2 3) '(4 5 6)) => (
 ;; (nconc '(a b c) '(d e f)) => (a b c d
 ;; (nconc '() '(a b c)) => (a b c)
 (defun nconc (list1 list2)
  ;; Base case: if either list is empty, return the other list
  (if (or (null list1) (null list2))
      list2
      ;; Recursive case: combine the first element of the first list with the rest of the
      ;; nconc of the first list and the rest of the second list
      (cons (car list1) (nconc (cdr list1) list2))))

;; although it would be even easier to use randomize:
(randomize (sequence 1 99))
;-> (54 38 91 18 76 71 19 30 ...

;; 4.1.18 random: generating a random number
;; The random function generates a random number between 0 (inclusive) and 1 (exclusive).

 ;; Example:
 ;; (random) => 0.625
 ;; (random) => 0.278912
 ;; (random) => 0.897841
 (defun random ()
  (expt 2 (- (random 1) 1)))

;; 4.1.19 random-integer: generating a random integer
;; The random-integer function generates a random integer between two specified integers,
;; inclusive.

 ;; Example:
 ;; (random-integer 1 10) => 1
 ;; (random-integer 1 10) => 3
 ;; (random-integer 1 10) => 5
 (defun random-integer (low high)
  (+ low (random (* (- high low) (random 1)))))

;; (That's one of the nice things about newLISP - a more elegant solution is just a re-write
;; away!)


;; 4.1.20 sort: sorting a list
;; The sort function sorts a list in ascending order.

 ;; Example:

 ;; (sort '(10 5 8 3 7)) => (3 5
 ;; (sort '(a b c d e)) => (a b c d e)
 ;; (sort '()) => '()
 (defun sort (list)
  ;; Base case: if the list has only one element, return the list unchanged
  (if (<= (length list) 1)
      list
      ;; Recursive case: find the minimum element in the list and remove it,
      ;; then sort the rest of the list and concatenate the minimum element with the sorted rest
      (cons (car (sort (remove (car list) list))) (sort (remove (car
       list) list))))))


;; 4.1.21 remove: removing an element from a list
;; The remove function removes an element from a list.

 ;; Example:
 ;; (remove 5 '(1 2 5 3 5 4 5))
 ;; => (1 2 3 4 5)
 ;; (remove 'a '(b a c d e))
 ;; => (b c d e)
 ;; (remove 'a '())
 ;; => '()
 (defun remove (element list)
  ;; Base case: if the list is empty, return the list unchanged
  (if (null list)
      list
      ;; Recursive case: if the first element of the list is equal to the element to remove,
      ;; remove it and concatenate the rest of the list with the recursive call to remove
      ;; the element from the rest of the list
      (if (eq element (car list))
       (cdr (remove element (cdr list)))
       (cons (car list) (remove element (cdr list))))))


;; 4.1.22 length: calculating the length of a list
;; The length function calculates the length of a list.

 ;; Example:

 ;; (length '(1 2 3 4 5)) => 5
 ;; (length '(a b c d e)) => 5
 ;; (length '()) => 0
 (defun length (list)
  ;; Base case: if the list is empty, return 0
  (if (null list)
      0
      ;; Recursive case: add 1 to the length of the rest of the list
      (+ 1 (length (cdr list)))))


;; 4.1.23 car: retrieving the first element of a list
;; The car function retrieves the first element of a list.

 ;; Example:
 ;; (car '(1 2 3 4 5)) => 1
 ;; (car '(a b c d e)) => a
 ;; (car '()) => nil
 (defun car (list)
  ;; Base case: if the list is empty, return nil
  (if (null list)
   nil
   ;; Recursive case: return the first element of the list
   (car (cdr list))))


;; 4.1.24 cdr: retrieving the rest of a list
;; The cdr function retrieves the rest of a list.

 ;; Example:
 ;; (cdr '(1 2 3 4 5)) => (2 3
 ;; (cdr '(a b c d e)) => (b c d e)
 ;; (cdr '()) => nil
 (defun cdr (list)
  ;; Base case: if the list is empty, return nil
  (if (null list)
   nil
   ;; Recursive case: return the rest of the list
   (cdr (cdr list))))


;; push has an opposite, pop, which destructively removes an element from a list, returning
;; the removed element. We'll meet pop and other list surgery functions later. See List
;; surgery1 .

;; These two functions, like many other newLISP functions, work on strings as well as lists.
;; See push and pop work on strings too2 .


;; 4.1.26 string-length: calculating the length of a string
;; The string-length function calculates the length of a string.

 ;; Example:
 ;; (string-length "hello") => 5
 ;; (string-length "world") => 5
 ;; (string-length "") => 0
 (defun string-length (string)
  ;; Base case: if the string is empty, return 0
  (if (null string)
      0
      ;; Recursive case: add 1 to the length of the rest of the string
      (+ 1 (string-length (cdr string)))))


;; 4.1.27 string-ref: retrieving a character from a string
;; The string-ref function retrieves a character from a string.

 ;; Example:
 ;; (string-ref "hello" 0) => h
 ;; (string-ref "world" 4) => o
 ;; (string-ref "hello" 5) => nil
 (defun string-ref (string index)
  ;; Base case: if the index is out of range, return nil
  (if (<= index (- (string-length string) 1))
   nil
   ;; Recursive case: return the character at the given index
   (char (string-ref string index))))


;; 4.1.28 string-set!: changing a character in a string
;; The string-set! function changes a character in a string.

 ;; Example:
 ;; (string-set! "hello" 0 'x) => "xello"
 ;; (string-set! "world" 4 'z) => "worldz"
 (defun string-set! (string index new-char)
  ;; Base case: if the index is out of range, do nothing
  (if (<= index (- (string-length string) 1))
   ;; Recursive case: create a new string by replacing the character at the given index with the new character
   (string (string-ref string 0) (string-set! (cdr string) index
    new-char))))

;; 4.1.3 dup: building lists of duplicate elements
;; A useful function called dup lets you construct lists quickly by repeating elements a given
;; number of times:

(dup 1 6)
;-> (1 1 1 1 1 1)
; duplicate 1 six times
(dup '(1 2 3) 6)
;-> ((1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3) (1 2 3))
(dup x 6)
;-> (x x x x x x)


;; 4.1.30 reverse: reversing a list
;; The reverse function reverses a list.

 ;; Example:
 ;; (reverse '(1 2 3 4 5)) => (5 4
 ;; (reverse '(a b c d e)) => (e d c b a)
 ;; (reverse '()) => '()
 (defun reverse (list)
  ;; Base case: if the list is empty, return the list unchanged
  (if (null list)
      list
      ;; Recursive case: concatenate the last element of the list with the reverse of the rest of the list
      (cons (car list) (reverse (cdr list)))))
    
;; 4.1.31 append: combining lists
;; The append function combines two lists into a single list.

 ;; Example:
 ;; (append '(1 2 3) '(4 5 6)) => (1
 ;; (append '(a b c) '(d e f)) => (a b c d e
 ;; (append '() '(a b c)) => (a b c)
 (defun append (list1 list2)
  ;; Base case: if either list is empty, return the other list unchanged
  (if (null list1)
      list2
      ;; Recursive case: concatenate the first element of the first list with the result of appending the rest
      ;; of the first list to the rest of the second list
      (cons (car list1) (append (cdr list1) list2))))



;; 4.1.32 list: creating a list from a series of arguments
;; The list function creates a list from a series of arguments.

 ;; Example:
 ;; (list 1 2 3) => (1 2 3)
 ;; (list 'a 'b 'c) => (a b c)
 ;; (list) => ()
 (defun list (&rest args)
  args)



;; 4.1.33 length: calculating the length of a list
;; The length function calculates the length of a list.

 ;; Example:
 ;; (length '(1 2 3 4 5)) => 5
 ;; (length '(a b c d e)) => 5
 ;; (length '()) => 0
 (defun length (list)
  ;; Base case: if the list is empty, return 0
  (if (null list)
      0
      ;; Recursive case: add 1 to the length of the rest of the list
      (+ 1 (length (cdr list)))))


;; 4.1.34 car: retrieving the first element of a list
;; The car function retrieves the first element of a list.

 ;; Example:
 ;; (car '(1 2 3 4 5)) => 1
 ;; (car '(a b c d e)) => a
 ;; (car '()) => nil
 (defun car (list)
  ;; Base case: if the list is empty, return nil
  (if (null list)
   nil
   ;; Recursive case: return the first element of the list
   (car (cdr list))))

;; There's a trick to get dup to return a list of strings. Because dup can also be used to
;; duplicate strings into a single longer string, you supply an extra true value at the end of
;; the list, and newLISP creates a list of strings rather than a string of strings.

(dup "x" 6)
;-> "xxxxxx"
; a string of strings
(dup "x" 6 true)
; a list of strings
;-> ("x" "x" "x" "x" "x" "x")


;; 4.2 Working with whole lists

;; Once you've got a list, you can start to work on it. First, let's look at the functions that
;; operate on the list as a unit. After that, I'll look at the functions that let you carry out list
;; surgery - operations on individual list elements.

;; 4.2.1 Using and processing lists
;;dolist works through every item of a list:

(set 'vowels '("a" "e" "i" "o" "u"))
    (dolist (v vowels)
    (println (apply upper-case (list v))))


;; 4.2.2 map: applying a function to every element of a list
;; map applies a function to every element of a list, returning a new list with the results:

;; In this example, apply expects a function and a list, and uses the elements of that list as
;; arguments to the function. So it repeatedly applies the upper-case function to the loop
;; variable's value in v. Since upper-case works on strings but apply expects a list, I've had
;; to use the list function to convert the current value of v (a string) in each iteration to a
;; list.
;; A better way to do this is to use map:

(map upper-case '("a" "e" "i" "o" "u"))
;-> ("A" "E" "I" "O" "U")

;; 4.2.3 filter: selecting elements of a list based on a condition
;; filter selects elements of a list based on a condition.

 ;; In this example, filter expects a function and a list, and returns a new list with only
 ;; the elements for which the function returns true:
 (filter even? '(1 2 3 4 5))
 ;-> (2 4)


 ;; 4.2.4 fold: combining a list with a function
 ;; fold combines a list with a function to produce a single value.
 ;; In this example, fold expects a function, a list, and an initial value. It repeatedly
 ;; applies the function to the current value and the accumulator, and updates the accumulator
 ;; with the result. In this case, fold uses the addition function to combine the list of
 ;; numbers (1 2 3 4 5) with the initial value of 0
 (fold + '(1 2 3 4 5) 0)
 ;-> 15


;; 4.2.5 sort: sorting a list
;; sort sorts a list in ascending order.

 ;; In this example, sort expects a list and returns a new list with the same elements,

 ;; but in sorted order:
 (sort '(5 3 1 4 2))
 ;-> (1 2 3 4 5)

 ;; 4.2.6 remove: removing elements from a list
 ;; remove removes elements from a list based on a condition.
 ;; In this example, remove expects a function and a list, and returns a new list with only
 ;; the elements for which the function returns false:
 (remove odd? '(1 2 3 4 5))
 ;-> (2 4)


 ;; 4.2.7 reverse-list: reversing a list
 ;; reverse-list reverses a list.
 ;; In this example, reverse-list expects a list and returns a new list with the same
 ;; elements, but in reverse order:
 (reverse-list '(1 2 3 4 5))
 ;-> (5 4 3 2 1)

;; map applies the named function, upper-case in this example, to each item of the list in
;; turn. The advantage of map is that it both traverses the list and applies the function to
;; each item of the list in one pass. The result is a list, too, which might be more useful for
;; subsequent processing.
;; There's more about dolist and apply elsewhere (see Working through a list3 , and Apply
;; and map: applying functions to lists4 ).


;; 4.3 Working with lists as data structures


;; 4.3.1 Lists as stacks
;; Lists can be used as stacks. In this example, I create a stack of integers, push
;; integers onto the stack, and then pop integers off the stack:
 (let ((stack '()))
   ;; Push integers onto the stack
   (dolist (i '(1 2 3 4 5))
    (push i stack))
   ;; Pop integers off the stack
   (dolist (i stack)
    (println i))
   )
   ;-> 5 4 3 2 1



;; 4.3.2 Lists as queues
;; Lists can be used as queues. In this example, I create a queue of integers, enqueue
;; integers onto the queue, and then dequeue integers from the queue:
 (let ((queue '()))
   ;; Enqueue integers onto the queue
   (dolist (i '(1 2 3 4 5))
    (enqueue i queue))
   ;; Dequeue integers from the queue
   (dolist (i queue)
    (println i))
   )
   ;-> 1 2 3 4 5

;; 4.2.2 reverse
;; reverse does what you would expect, and reverses the list. It's a destructive function,
;; changing the list permanently.

(reverse '("A" "E" "I" "O" "U"))
;-> ("U" "O" "I" "E" "A")

;; 4.2.3 sort and randomize

;; In a way, randomize and sort are complementary, although sort changes the original
;; list, whereas randomize returns a disordered copy of the original list. sort arranges the
;; elements in the list into ascending order, organizing them by type as well as by value.
;; Here's an example: create a list of letters and a list of numbers, stick them together, shuﬄe
;; the result, and then sort it again:

(for (c (char "a") (char "z"))
(push (char c) alphabet -1))

(for (i 1 26)
(push i numbers -1))

(set 'data (append alphabet numbers))

;-> ("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p"
; "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" 1 2 3 4 5 6 7 8 9 10 11 12
; 13 14 15 16 17 18 19 20 21 22 23 24 25 26)

(shuffle data)
;-> ("t" "u" "s" "m" "j" "o"
; "n" "r" "p" "l" "k" "q" "
; "a" "b" "c" "d" "e" "f" "
; "g" "h" "i" "x" "w" "v" "
; "y" "z" 1 2 3 4 5 6
; 7 8 9 10 11 12 13
; 14 15 16 17 18 19
; 20 21 22 23 24 25
; 26)

(sort data)
;-> ("a" "b" "c" "d" "e" "f"
; "g" "h" "i" "j" "k" "l" "
; "m" "n" "o" "p" "q" "r" "
; "s" "t" "u" "v" "w" "x" "
; "y" "z" 1 2 3 4 5 6
; 7 8 9 10 11 12 13
; 14 15 16 17 18 19
; 20 21 22 23 24 25


;; 4.3.4 More about lists

;; 4.3.4.1 Nested lists
;; Nested lists are lists that contain other lists as elements. Here's an example:

 (let ((nested '((1 2) (3 4) (5 6))))
  (dolist (sublist nested)
   (dolist (i sublist)
    (print i " ")))
   )
   ;-> 1 2 3 4 5 6

(randomize data)
;-> ("l" "r" "f" "k" 17 10 "u" "e" 6 "j" 11 15 "s" 2 22 "d" "q" "b"
; "m" 19 3 5 23 "v" "c" "w" 24 13 21 "a" 4 20 "i" "p" "n" "y" 14 "g"
; 25 1 8 18 12 "o" "x" "t" 7 16 "z" 9 "h" 26)

(sort data)
;-> (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
; 25 26 "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
; "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

;; 4.3.4.2 List comprehensions
;; List comprehensions are a concise way to create lists based on existing lists. Here's an example
;; that creates a list of squares of numbers from 1 to 10:

 (let ((squares (map (lambda (x) (* x x)) (range 1
 (1+ 10)))))
  squares)
   ;-> (1 4 9 16 25 36 49

;; c5. More about Common Lisp

;; c5.1. The Common Lisp Object System
;; c5.1.1 Objects and classes
;; Objects are instances of classes. Classes define the behavior and properties of objects.
;; c5.1.2 Creating objects
;; The simplest way to create an object is to use the make-instance function, which takes two
;; arguments: the class and a list of initial values for the object's slots. Here's an
;; example:
 (let ((o (make-instance 'person :name "Alice" :age 25
 :gender 'female)))
  (print o)
   ;-> #<person: name "Alice", age 25, gender :female>


;; c5.1.3 Modifying objects
;; Objects can be modified using the slot-value-setf function, which takes three arguments:
;; the object, the slot name, and the new value for the slot. Here's an example
 (slot-value-setf o 'name "Bob")
 (print o)
  ;-> #<person: name "Bob", age 25, gender :female>
  (slot-value-setf o 'age 30)
  (print o)
   ;-> #<person: name "Bob", age 30, gender :female>


;; c5.1.4 Defining classes
;; Classes can be defined using the defclass function, which takes three arguments: the
;; class name, a list of superclasses, and a list of slots. Here's an example
 (defclass person ()
  ((name :initarg :name :accessor name)
   (age :initarg :age :accessor age)
   (gender :initarg :gender :accessor gender)))
   ;-> :person


;; c5.1.5 Defining methods
;; Methods can be defined using the defmethod function, which takes three arguments: the
;; class, the slot name, and a function that implements the method. Here's an example
 (defmethod print-person ((person person))
  (format t "~a is a ~a, age ~a~%" person (class-of
   person) (age person)))
   ;-> :print-person


;; 5.2. The Common Lisp Reader
;; 5.2.1 Reading expressions
;; The Common Lisp reader reads expressions from strings and returns them as objects.
;; 5.2.2 Reading from strings
;; The read function reads an expression from a string and returns it as an object.
;; 5.2.3 Reading from files
;; The read-from-string function reads an expression from a string and returns it as an


;; 5.3. The Common Lisp Writer
;; 5.3.1 Writing expressions
;; The Common Lisp writer writes objects to strings and returns them as strings.
;; 5.3.2 Writing to strings
;; The write function writes an object to a string and returns it as a string.
;; 5.3.3 Writing to files
;; The write-to-string function writes an object to a string and returns it as a string

;; 5.4. The Common Lisp Compiler
;; 5.4.1 Compiling expressions
;; The Common Lisp compiler compiles expressions to bytecodes and returns them as objects.
;; 5.4.2 Compiling to bytecodes
;; The compile function compiles an expression to bytecodes and returns them as objects.
;; 5.4.3 Compiling to source code
;; The compile-to-string function compiles an expression to source code and returns it as a

;; 5.5. The Common Lisp Debugger
;; 5.5.1 Debugging expressions
;; The Common Lisp debugger allows you to step through expressions, inspect variables, and


;; 5.6. The Common Lisp Standard Library
;; 5.6.1 Core functions and data structures
;; The Common Lisp standard library provides a wide range of core functions and data
;; structures, such as lists, vectors, hash tables, and random number generators.
;; 5.6.2 Extending the standard library
;; The Common Lisp standard library is extensible, allowing you to add new functions and
;; data structures to it.
;; 5.6.3 Loading and saving libraries
;; The Common Lisp standard library provides functions for loading and saving libraries,
;; allowing you to reuse and share code.

;; 5.7. The Common Lisp System
;; 5.7.1 The environment
;; The Common Lisp environment is a collection of variables, functions, and other
;; objects that make up a program.
;; 5.7.2 Loading and evaluating code
;; The Common Lisp system provides functions for loading and evaluating code, such as
;; load and eval.
;; 5.7.3 Modifying the environment
;; The Common Lisp system provides functions for modifying the environment, such as
;; define-variable, set-variable-value, and remove-variable.

;; 5.8. The Common Lisp User Interface
;; 5.8.1 Creating a simple GUI
;; The Common Lisp user interface library provides functions for creating simple graphical
;; user interfaces, such as buttons, labels, and windows.
;; 5.9. The Common Lisp Networking Library
;; 5.9.1 Creating a simple network client
;; The Common Lisp networking library provides functions for creating simple network
;; clients, such as TCP and UDP sockets.
;; 5.10. The Common Lisp Documentation System
;; 5.10.1 Creating a simple documentation system
;; The Common Lisp documentation system provides functions for creating simple


;; 5.11. The Common Lisp System Management
;; 5.11.1 Starting and stopping the Common Lisp system
;; The Common Lisp system management library provides functions for starting and stopping
;; the Common Lisp system, such as start-lisp and stop-lisp.
;; 5.12. The Common Lisp Performance Tuning
;; 5.12.1 Optimizing Common Lisp code
;; The Common Lisp performance tuning library provides functions for optimizing
;; 5.13. The Common Lisp System Configuration
;; 5.13.1 Configuring the Common Lisp system
;; The Common Lisp system configuration library provides functions for configuring
;; the Common Lisp system, such as setenv and getenv.

;; 5.14. The Common Lisp System Monitoring
;; 5.14.1 Monitoring the Common Lisp system
;; The Common Lisp system monitoring library provides functions for monitoring
;; the Common Lisp system, such as trace and untrace.
;; 5.15. The Common Lisp System Error Handling
;; 5.15.1 Handling Common Lisp errors
;; The Common Lisp system error handling library provides functions for handling
;; Common Lisp errors, such as error, warn, and debug.
;; 5.16. The Common Lisp System Logging
;; 5.16.1 Logging Common Lisp events
;; The Common Lisp system logging library provides functions for logging
;; Common Lisp events, such as log and log-message.
;; 5.17. The Common Lisp System Internationalization
;; 5.17.1 Internationalizing Common Lisp code
;; The Common Lisp system internationalization library provides functions for
;; internationalizing Common Lisp code, such as format and format-message.
;; 5.18. The Common Lisp System Customization
;; 5.18.1 Customizing the Common Lisp system
;; The Common Lisp system customization library provides functions for
;; customizing the Common Lisp system, such as defcustom and customize-group.

;; 5.19. The Common Lisp System Extensibility
;; 5.19.1 Extending the Common Lisp system


;; 5.20. The Common Lisp System Development
;; 5.20.1 Developing Common Lisp code


;; 5.21. The Common Lisp System Testing
;; 5.21.1 Testing Common Lisp code

;; Compare data before it was randomized and after it was sorted. The sort command sorts
;; the list by data type as well as by value: integers before strings, strings before lists, and so
;; on.

;; The default sorting method is <, which arranges thet values so that each is less than the
;; next.

;; To change the sort method, you can supply one of newLISP's built-in comparison functions,
;; such as >. Adjacent objects are considered to be correctly sorted when the comparison
;; function is true for each adjacent pair:


(for (c (char "a") (char "z"))
(push (char c) alphabet -1))
alphabet
;-> ("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n"
; "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")

(sort alphabet >)
;-> ("z" "y" "x" "w" "v" "u" "t" "s" "r" "q" "p" "o" "n"
; "m" "l" "k" "j" "i" "h" "g" "f" "e" "d" "c" "b" "a")

;; You can supply a custom sort function. This is a function that takes two arguments and
;; returns true if they're in the right order (ie if the ﬁrst should come before the second), and
;; false if they aren't. For example, suppose you want to sort a list of ﬁle names so that the
;; shortest name appears ﬁrst. Deﬁne a function that returns true if the ﬁrst argument is
;; shorter than the second, and then use sort with this custom sorting function:


(defun short-file-name-p (x y)
(string-lessp (length x) (length y)))

;; Advanced newLISPers often write a nameless function and supply it directly to the sort
;; command:

(sort (directory) (fn (a b) (< (length a) (length b))))

;; This does the same job, but saves 25 characters or so. You can use either fn or lambda to
;; deﬁne an inline or anonymous function.


;; 5.3.1 Reading from strings
;; The read function reads an expression from a string and returns it as an object.
;; For example:

(read-from-string "(+ 1 2)")
;-> (+ 1 2)


;; 5.3.2 Reading from files
;; The read-from-string function reads an expression from a string and returns it as an
;; object.
;; For example:

(with-open (stream "example.lisp")
  (read-from-string (with-input-from-stream stream nil)))
  ;-> (defun hello () (print "Hello, world!"))
  ;-> hello
  ;-> nil


;; 5.4.1 Compiling expressions
;; The compile function compiles an expression to bytecodes and returns them as objects.
;; For example:

(compile '(+ 1 2))
;-> #<compiled-function #x200000000000


;; 5.4.2 Compiling to bytecodes
;; The compile-to-string function compiles an expression to bytecodes and returns them as a
;; string.
;; For example:

(compile-to-string '(+ 1 2))
;-> "(defun #x200000000000 () (+
; 1
; 2))"

;; 5.4.3 Compiling to source code
;; The compile-to-string function compiles an expression to source code and returns it as a
;; string.
;; For example:

(compile-to-string '(+ 1 2))
;-> "(defun #x200000000000 () (+
; 1
; 2))"

;; 5.5.1 Debugging expressions
;; The Common Lisp debugger allows you to step through expressions, inspect variables, and
;; set breakpoints.
;; To start the debugger, use the debugger function:

(debugger '(+ 1 2))
;-> Debugger entered--you can now use the commands 'step', 'next', 'continue',
; 'backtrace', 'up', 'down', 'return', 'quit', and 'eval'
;-> (+ 1 2)


