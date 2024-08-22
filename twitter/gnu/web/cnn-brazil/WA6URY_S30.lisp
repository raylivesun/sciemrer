;; Here I'm looking for any traces of chemical indulgence in Sherlock Holmes' bohemian way of
;; life: "(cookies|coakepaste|chocolate|pizza)" means any one of tea, cokepaste, chocolate, or pizza.
;; This form of ﬁnd lets you look for regular expression patterns in the string elements of a list.
;; You'll meet regular expressions again when we explore strings. See Regular expressions17 .


(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))


;; Now, let's test this function with our data.
(find-chemical-indulgence records)


;; Output:
;; [{:bohemian "yes", :date "1897-09-
;;   :location "London", :name "John Watson", :occupation "Private
;;   Secretary", :residence "London", :sex "Male"}]


;; Here we're looking for any string elements in the word-list that match our pattern (a c
;; followed by ie, the old and inaccurate spelling rule i before e except after c).
;; The regular expression pattern here (enclosed in braces, which are string delimiters, doing
;; more or less the same job as quotation marks) is (c) followed by (ie). Then there's a
;; comment, starting with (?#. Comments in regular expressions are useful when things get
;; cryptic, as they often do.

;; ﬁnd can also accept a comparison function. See Searching lists18 .
;; ﬁnd ﬁnds only the ﬁrst occurrence in a list. To ﬁnd all occurrences, you could keep repeating
;; ﬁnd until it returns nil. The word list gets shorter each time through, and the found
;; elements are added at the end of another list:


(defn find-all-chemical-indulgence [records]
  (reduce (fn [acc record]
   (if (re-find #"(cookies|coakepaste|chocolate|pizza)
    (string/lower-case (:bohemian record))
     acc (cons record acc))) [] records))
     ;; [] is the initial accumulator, and records is the list to search.
     ;; The result is a new list containing all records where bohemian indulgence is mentioned.
     ;; ﬁnd-all-chemical-indulgence records))
     ;; Output:
     ;; [{:bohemian "yes", :date "1897-09-
     ;;   :location "London", :name "John Watson", :occupation "Private
     ;;   Secretary", :residence "London", :sex "Male"}]
     ;; Note that the order of records in the output list might not match the order of
     ;; records in the input list.
     
;; 18. Searching lists

;; 18. Searching lists is a fundamental concept in functional programming. It's a

;; powerful tool for finding elements in lists, arrays, or other collections.
;; You'll meet functions like map, filter, and reduce, which are all used to transform
;; lists and create new ones. See Lists19 .
;; 19. Lists


;; Lists are a fundamental data structure in functional programming. They're like arrays
;; in other languages, but they're immutable, meaning you can't change the elements
;; of a list once it's created. Lists are denoted with square brackets, and the elements
;; are separated by commas. Here are some examples of lists:
;; []  An empty list.
;; [1]  A list with one element.
;; [1, 2, 3]  A list with three elements.

;; 20. Lists are also called sequences, as they have a 0-based index
;; (the first element is at index 0, the second at index 1, and so
;; forth).

;; 21. Lists are a fundamental concept in functional programming. They're a
;; powerful tool for finding elements in lists, arrays, or other collections.
;; You'll meet functions like map, filter, and reduce, which are all used to transform
;; lists and create new ones. See Lists22 .

;; 22. Lists

;; Lists are a fundamental data structure in functional programming. They're like arrays
;; in other languages, but they're immutable, meaning you can't change the elements
;; of a list once it's created. Lists are denoted with square brackets, and the elements
;; are separated by commas. Here are some examples of lists:


;; []  An empty list.
;; [1]  A list with one element.
;; [1, 2, 3]  A list with three elements.

;; 23. Lists are also called sequences, as they have a 0-based index
;; (the first element is at index 0, the second at index 1, and so
;; forth).

;; 24. Lists are a fundamental concept in functional programming. They're a
;; powerful tool for finding elements in lists, arrays, or other collections.
;; You'll meet functions like map, filter, and reduce, which are all used to transform
;; lists and create new ones. See Lists25 .

;; 25. Lists

;; Lists are a fundamental data structure in functional programming. They're like arrays
;; in other languages, but they're immutable, meaning you can't change the elements
;; of a list once it's created. Lists are denoted with square brackets, and the elements
;; are separated by commas. Here are some examples of lists:

;; []  An empty list.
;; [1]  A list with one element.
;; [1, 2, 3]  A list with three elements.


(set 'word-list '("scientist" "being" "believe" "ceiling" "conceit"
     "conceive" "deceive" "financier" "foreign" "neither" "receive" "science"
     "sufficient" "their" "vein" "weird"))

(while (set 'temp
       (find {(c)(ie)(?# i before e except after c...)} word-list 0))
       (push (word-list temp) results -1)
       (set 'word-list ((+ temp 1) word-list)))

results
;-> ("scientist" "financier" "science" "sufficient")



(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a
   ; c followed by ie, the old and inaccurate spelling rule i before e except after
   ; c). The regular expression pattern here (enclosed in braces, which are string
   ; delimiters, doing more or less the same job as quotation marks) is (c)
   ; followed by (ie). Then there's a comment, starting with (?#. Comments in regular
   ; expressions are useful when things get cryptic, as they often do.
   
; 21. Searching lists

; 22. Lists

; Lists are a fundamental data structure in functional programming. They're like arrays
; in other languages, but they're immutable, meaning you can't change the elements
; of a list once it's created. Lists are denoted with square brackets, and the elements
; are separated by commas. Here are some examples of lists:

;; []  An empty list.
;; [1]  A list with one element.
;; [1, 2, 3]  A list with three elements.

(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a

;; But in this case it's much easier to use ﬁlter:

(filter (fn (w) (find {(c)(ie)} w 0)) word-list)
;-> ("scientist" "financier" "science" "sufficient")


(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a
   ; c followed by ie, the old and inaccurate spelling rule i before e except after
   ; c). The regular expression pattern here (enclosed in braces, which are string
   ; delimiters, doing more or less the same job as quotation marks) is (c)
   ; followed by (ie). Then there's a comment, starting with (?#. Comments in regular
   ; expressions are useful when things get cryptic, as they often do.

; 22. Lists

; Lists are a fundamental data structure in functional programming. They're like arrays
; in other languages, but they're immutable, meaning you can't change the elements
; of a list once it's created. Lists are denoted with square brackets, and the elements
; are separated by commas. Here are some examples of lists:

;; []  An empty list.
;; [1]  A list with one element.
;; [1, 2, 3]  A list with three elements.

(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a
   ; c followed by ie, the old and inaccurate spelling rule i before e except after
   ; c). The regular expression pattern here (enclosed in braces, which are string
   ; delimiters, doing more or less the same job as quotation marks) is (c)
   ; followed by (ie). Then there's a comment, starting with (?#. Comments in regular
   ; expressions are useful when things get cryptic, as they often do.

;; The list of results produced by count shows how many times each element in the ﬁrst list
;; occurs in the second list, so there are 34 mentions of Sherlock, 135 mentions of Holmes, 24
;; of Watson, and only one mention for poor old Inspector Lestrade in this story.
;; It's worth knowing that ﬁnd examines the list only superﬁcially. For example, if the list
;; contains nested lists, you should use ref rather than ﬁnd, because ref looks inside sublists:

(set 'maze
'((1 2)
 (1 2 3)


(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))


find 4 maze)
;-> nil; didn't look inside the lists
(ref 4 maze)
;-> (2 3); element 3 of element 2


;; 4.3.2 member

;; The member function returns the rest of the source list rather than index numbers or
;; counts:

(set 's (sequence 1 100 7))
; 7 times table?
;-> (1 8 15 22 29 36 43 50 57 64 71 78 85 92 99)
(member 78 s)
;-> (78 85 92 99)


;; 4.3.3 rest

;; The rest function returns the rest of the source list, starting from the nth element:

(set 's (sequence 1 100 7))
; 7 times table?
;-> (1 8 15 22 29 36 4
(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a
   ; c followed by ie, the old and inaccurate spelling rule i before e except after
   ; c). The regular expression pattern here (enclosed in braces, which are string
   ; delimiters, doing more or less the same job as quotation marks) is (c)


;; 4.3.3 rest

;; The rest function returns the rest of the source list, starting from the nth element:

(set 's (sequence 1 100 7))
; 7 times table?
;-> (1 8 15 22 29 36 4
(defn find-chemical-indulgence [records]
  (filter #(re-find #"(cookies|coakepaste|chocolate|pizza)
   (string/lower-case (:bohemian %))) records))
   ; Now, let's test this function with our data.
   (find-chemical-indulgence records)
   ; Output:
   ; [{:bohemian "yes", :date "1897-09-
   ;   :location "London", :name "John Watson", :occupation "Private
   ;   Secretary", :residence "London", :sex "Male"}]
   ; Here we're looking for any string elements in the word-list that match our pattern (a
   ; c followed by ie, the old and inaccurate spelling rule i before e except after
   ; c). The regular expression pattern here (enclosed in braces, which are string
   ; delimiters, doing more or less the same job as quotation marks) is (c)
   ; followed by (ie). Then there's a comment, starting with (?#. Comments in regular
   ; expressions are useful when things get cryptic, as they often do.
   ; 4.3.3 rest
   ; The rest function returns the rest of the source list, starting from the nth element:
   
   (set (sequence 1 100 7))


;; 4.3.3 matching patterns in lists

;; There's a powerful and complicated function called match, which looks for patterns in
;; lists. It accepts the wild card characters *, ?, and +, which you use to deﬁne a pattern of
;; elements. + means one or more elements, * means zero or more elements, and ? means one
;; element. For example, suppose you want to examine a list of random digits between 0 and
;; 9 for patterns. First, generate a list of 10000 random numbers as source data:

(dotimes (c 10000) (push (rand 10) data))
;-> (7 9 3 8 0 2 4 8 3 ...)


;; Now, use match to look for patterns in the data:


;; 4.3.3 matching patterns in lists

;; There's a powerful and complicated function called match, which looks for patterns in
;; lists. It accepts the wild card characters *,?, and +, which you use to de�
;; fine a pattern of elements. + means one or more elements, * means zero or more elements
;; and? means one element. For example, suppose you want to examine a list of random digits
;; between 0 and 9 for patterns. First, generate a list of 100

;; somewhere in the list, ie anything followed by 1 followed by a 2 followed by a 3 followed by
;; anything. Call match like this:

(match '(* 1 2 3 *) data)

;; which looks odd, but it's just a pattern speciﬁcation in a list, followed by the source data.
;; The list pattern:

(* 1 2 3)

;; means any sequence of atoms or expressions (or nothing), followed by a 1, then a 2, then a
;; 3, followed by any number of atoms or expressions or nothing. The answer returned by this
;; match function is another list, consisting of two sublists, one corresponding to the ﬁrst *,
;; the other corresponding to the second:

((7 9 3 8
. . .
0 4 5)
(7 2 4 1 . . . 3 5 5 5))

;; and the pattern you were looking for ﬁrst occurred in the gap between these lists (in fact it
;; occurs half a dozen times later on in the list as well). match can also handle nested lists.

;; To ﬁnd all occurrences of a pattern, not just the ﬁrst, you can use match in a while loop.
;; For example, to ﬁnd and remove every 0 when it's followed by another 0, repeat the match
;; on a new version of the list until it stops returning a non-nil value:

(set 'number-list '(2 4 0 0 4 5 4 0 3 6 2 3 0 0 2 0 0 3 3 4 2 0 0 2))
(while (set 'temp-list (match '(* 0 0 *) number-list))
(println temp-list)
(set 'number-list (apply append temp-list)))
((2 4) (4 5 4 0 3 6 2 3 0 0 2 0 0 3 3 4 2 0 0 2))
((2 4 4 5 4 0 3 6 2 3) (2 0 0 3 3 4 2 0 0 2))
((2 4 4 5 4 0 3 6 2 3 2) (3 3 4 2 0 0 2))
((2 4 4 5 4 0 3 6 2 3 2 3 3 4 2) (2))
number-list
;-> (2 4 4 5 4 0 3 6 2 3 2 3 3 4 2 2)


;; 4.3.4 string matching


;; 4.3.4 string matching

;; There are several functions in Clojure to match strings, including:

;; You don't have to ﬁnd elements ﬁrst before replacing them: just use replace, which does the
;; ﬁnding and replacing in one operation. And you can use match as a comparison function
;; for searching lists. See Replacing information: replace21 , and Searching lists22 .


;; 4.3.4 string matching
;; There are several functions in Clojure to match strings, including:


;; 4.3.4 string matching
;; You don't have to ﬁnd elements ﬁrst before replacing them
;; just use replace, which does the ﬁnding and replacing in one operation.
;; And you can use match as a comparison function
;; for searching lists. See Replacing information: replace21 , and Searching lists2

;; 4.3.4 ﬁnd-all

;; ﬁnd-all is a powerful function with a number of diﬀerent forms, suitable for searching lists,
;; association lists, and strings. For list searches, you supply four arguments: the search key,
;; the list, an action expression, and the functor, which is the comparison function you want
;; to use for matching the search key:

(set 'food '("bread" "cheese" "onion" "pickle" "lettuce"))
(find-all "onion" food (print $0 { }) >)
;-> bread cheese lettuce


;; Here, ﬁnd-all is looking for the string "onion" in the list food. It's using the > function
;; as a comparison function, so it will ﬁnd anything that "onion" is greater than. For strings,
;; 'greater than' means appearing later in the default ASCII sorting order, so that "cheese"
;; is greater than "bread" but less than "onion". Notice that, unlike other functions that let
;; you provide comparison functions (namely ﬁnd, ref, ref-all, replace when used with lists,
;; set-ref, set-ref-all, and sort), the comparison function must be supplied. With the <
;; function, the result is a list of things that "onion" is less than:

(find-all "onion" food (print $0 { }) <)
;-> pickle

;; 4.3.4 ﬁnd-index


;; 4.3.4 ﬁnd-index
;; ﬁnd-index is another powerful function with a number of diﬀ
;; forms, suitable for searching lists, association lists, and strings. For list searches, you
;; supply four arguments: the search key, the list, an action expression, and the functor,
;; which is the comparison function you want to use for matching the search key:


;; 4.3.5 ref and ref-all

;; The ref function returns the index of the ﬁrst occurrence of an element in a list. It's
;; particularly suited for use with nested lists, because, unlike ﬁnd, it looks inside all the
;; sublists, and returns the address of the ﬁrst appearance of an element. As an example,


;; suppose you've converted an XML ﬁle, such as your iTunes library, into a large nested list,
;; using newLISP's built-in XML parser:


(xml-type-tags nil nil nil nil)
; controls XML parsing
(set 'itunes-data
(xml-parse
(read-file "/Users/me/Music/iTunes/iTunes Music Library.xml")
(+ 1 2 4 8 16)))

;; Now you can look for any expression in the data, which is in the form of an ordinary
;; newLISP list:

(ref "Brian Eno" itunes-data)


;; and the returned list will be the location of the ﬁrst occurrence of that string in the list:

(0 2 14 528 6 1)

;; - this is a set of index numbers which together deﬁne a kind of address. This example
;; means: in list element 0, look for sublist element 2, then ﬁnd sublist element 14 of that
;; sublist, and so on, drilling down into the highly-nested XML-based data structure. See
;; Working with XML23 .

;; Use these functions when you're searching for something in a nested list. If you want to
;; replace it when you've found it, use the set-ref and set-ref-all functions. See Find and
;; replace matching elements25 .


;; 4.3.6 Filtering lists: ﬁlter, clean, and index

;; Another way of ﬁnding things in lists is to ﬁlter the list. Like panning for gold, you can
;; create a ﬁlter that keeps only the stuﬀ you want, ﬂushing the unwanted stuﬀ away.
;; The functions ﬁlter and index have the same syntax, but ﬁlter returns the list elements,
;; whereas index returns the index numbers (indices) of the wanted elements rather than the
;; list elements themselves. (These functions don't work on nested lists.)
;; The ﬁltering functions ﬁlter, clean, and index use another function for testing elements:
;; the element appears in the results list according to whether it passes the test or not. You
;; can either use a built-in function or deﬁne your own. Typically, newLISP functions that
;; tests and return true or false (sometimes called predicate functions) have names ending
;; with question marks:


;; NaN? array? atom? context? directory? empty? ﬁle? ﬂoat? global? integer?
;; lambda? legal? list? macro? nil? null? number? primitive? protected? quote?
;; string? symbol? true? zero?

;; So, for example, an easy way to ﬁnd integers in (and remove ﬂoating-point numbers from)
;; a list is to use the integer? function with ﬁlter. Only integers pass through this ﬁlter:

(set 'data '(0 1 2 3 4.01 5 6 7 8 9.1 10))
    (filter integer? data)
;-> (0 1 2 3 5 6 7 8 10)

;; ﬁlter has a complementary function called clean which removes elements that satisfy the
;; test:

(set 'data '(0 1 2 3 4.01 5 6 7 8 9.1 10))
    (clean integer? data)
;-> (4.01 9.1)

;; Think of clean as getting rid of dirt - it gets rid of anything that passes the test. Think of
;; ﬁlter as panning for gold, keeping what passes the test.
;; This next ﬁlter ﬁnds all words in Conan Doyle's story The Empty House that contain the
;; letters pp. The ﬁlter is a lambda expression (a temporary function without a name) that
;; returns nil if the element doesn't contain pp. The list is a list of string elements generated
;; by parse, which breaks up a string into a list of smaller strings according to a pattern.


(set 'empty-house-text
    (parse
    (read-file "/Users/me/Sherlock-Holmes/the-empty-house.txt")
     {,\s*|\s+} 0))
    (filter (fn (s) (find "pp" s)) empty-house-text)


;; 4.3.7 Indexing: index and index-all


;; 4.3.7 indexing: index and index-all
;; index and index-all are like ﬁlter and clean, but they return
;; the index numbers of the elements that satisfy the test. For example, the index of the
;; ﬁrst occurrence of the string "onion" in the list food is:

(index "onion" food)
;-> 2


;; You can also use ﬁlter or clean for tidying up lists before using them - removing empty
;; strings produced by a parse operation, for example.
;; When would you use index rather than ﬁlter or clean? Well, use index when you later
;; want to access the list elements by index number rather than their values: we'll meet
;; functions for selecting list items by index in the next section. For example, whereas ref
;; found the index of only the ﬁrst occurrence, you could use index to return the index
;; numbers of every occurrence of an element.
;; If you have a predicate function that looks for a string in which the letter c is followed by
;; ie, you can use that function to search a list of matching strings:

(set 'word-list '("agencies" "being" "believe" "ceiling")

(define (i-before-e-after-c? wd)
        ; a predicate function
        (find {(c)(ie)(?# i before e after c...)} wd 0))
        (index i-before-e-after-c? word-list)
;-> (0 7 11 12)
; agencies, financier, science, sufficient



;; 4.3.8 Selecting list items by index

;; 4.3.8 selecting list items by index
;; newLISP provides several functions for selecting list items by index number. Here are
;; a few:

;; nth and nth-value:
;; nth returns the nth element of a list, and nth-value returns the nth element of a
;; association list. For example:


(nth 2 '(0 1 2 3 4 5))

;; Remember that lists can contain nested lists, and that some functions won't 
;; look inside the
;; sub-lists:

(set 'maze
'((1 2.1)
(1 2 3)
(1 2 3 4)))
(filter integer? maze)
;-> (); I was sure it had integers...
(filter list? maze)
;-> ((1 2.1) (1 2 3) (1 2 3 4)); ah yes, they're sublists!
(filter integer? (flat maze))
;-> (1 1 2 3 1 2 3 4); one way to do it...


;; rest and take:
;; rest returns a list containing all but the ﬁrst element of a list, and
;; take returns a list containing the ﬁrst n elements of a list. For example

(rest '(0 1 2 3 4 5))

;; 4.3.7 Testing lists

;; The exists and for-all functions check elements in a list to see if they pass a test.
;; exists returns either the ﬁrst element in the list that passes the test, or nil if none of them
;; do.

(exists string? '(1 2 3 4 5 6 "hello" 7))
;-> "hello"
(exists string? '(1 2 3 4 5 6 7))
;-> nil

;; for-all returns true if all elements in the list pass the test.

;; for-all returns either true or nil. If every list element passes the test, 
;; it returns true.

(for-all number? '(1 2 3 4 5 6 7))
;-> true
(for-all number? '("zero" 2 3 4 5 6 7))
;-> nil


;; 4.3.9 Lists as stacks and queues

;; 4.3.9 lists as stacks and queues
;; Lists are also very useful for implementing stacks and queues. A stack is a
;; last-in-first-out (LIFO) data structure, and a queue is a
;; ﬁrst-in-first-out (FIFO) data structure. Here are
;; functions for working with lists as stacks and queues:

;; push:
;; push adds an element to the front of a list, and returns the new list.
;; pop:
;; pop removes and returns the front element of a list, and returns the new list.
;; empty:
;; empty returns true if a list is empty, and false otherwise.
;; Examples:

(set stack '())
(set queue '())

    (define (push x stack)
        (cons x stack))

    (define (pop stack)
        (if (empty? stack)
            '()
            (let ((first (car stack)))
                (cdr stack))))

    (define (empty? stack)
    (null? stack))

    (define (enqueue x queue)
        (cons x queue))

    (define (dequeue queue)
        (if (empty? queue)
            '()
            (let ((first (car queue)))
                (cdr queue))))

;; 4.3.10 List manipulation functions
;; 4.3.10 list manipulation functions
;; newLISP provides several built-in functions for manipulating lists. Here are
;; append:
;; append adds two lists together, and returns the new list.
;; reverse:
;; reverse reverses a list, and returns the new list.
;; Examples:

(set 'list1 '(1 2 3))
(set 'list2 '(4 5 6))
(append list1 list2)
;;-> (1 2 3 4 5 6)
(reverse '(1 2 3 4 5 6))
;;-> (6 5 4 3 2 1)

;; 4.3.11 List transformation functions
;; 4.3.11 list transformation functions
;; newLISP provides several built-in functions for transforming lists. Here are
;; map:
;; map applies a function to each element of a list, and returns a new list containing the results
;; filter:
;; filter removes elements from a list that do not pass a test, and returns the new list
;; reduce:
;; reduce applies a function to all elements of a list, combining them in some way, and returns
;; the result.
;; Examples:


(set 'list '(1 2 3 4 5))
(map (lambda (x) (* x 2)) list)
;;-> (2 4 6 8 10)
(filter even? list)
;;-> (2 4)
(reduce + list)
;;-> 15

;; 4.3.12 List utilities
;; 4.3.12 list utilities
;; newLISP provides several built-in functions for working with lists. Here are
;; length:
;; length returns the number of elements in a list.
;; nth-to-last:
;; nth-to-last returns the nth element from the end of a list, counting from
;; 0.
;; Examples:

(set 'list '(1 2 3 4 5))
(length list)
;;-> 5
(nth-to-last 2 list)
;;-> 3


;; 4.3.13 List comprehensions
;; 4.3.13 list comprehensions
;; newLISP provides a concise way to create lists using list comprehensions.
;; Here are examples of list comprehensions:

;; The following example uses the < comparison function. ﬁnd looks for the ﬁrst element
;; that compares favourably with n, ie the ﬁrst element that n is less than. With a value of
;; 1002, the ﬁrst element that satisﬁes the test is 1003, the 3rd element of the list, and so the
;; returned value is 3.


(define (find-third-largest n-list)
    (let ((third-largest (find (lambda (x) (> x n)) (reverse n
     list))))
        (if (null? third-largest)
            '()
            third-largest)))
            (list third-largest)))
            (find-third-largest 1002 '(1000 100
            1001 1002 1003 1004
            ))
            ;;-> (3)

(set 'n 1002)
; find the first something that n is less than:
(find n s <)
;-> 3, the index of 1003 in s

;; 4.4.1 Writing new Lisp functions
;; 4.4.1 writing new Lisp functions
;; newLISP provides several ways to create new functions. Here are some examples:

;; Function declaration:
;; Function declaration is done using the define function. The function takes a name, a list of arguments
;; (including zero or more), and a body of code. For example:

(define (add x y)
    (+ x y))

;; Function call:
;; Function call is done by putting the function name followed by the arguments inside parentheses.


(add 2 3)
;;-> 5

;; Function definition:
;; Function definition is done using the lambda function. The lambda function takes a list of arguments
;; (including zero or more), and a body of code. For example:

(define (add x y)
    (+ x y))

;; Function call:
    (lambda (x y) (+ x y)))


;; The longer? function returns true if the ﬁrst argument is longer than the second. So ﬁnd,
;; with this function as the comparison, ﬁnds the ﬁrst element in the list that makes the
;; comparison true. Because tiger is longer than dog, the function returns 3, the index of dog
;; in the list.

;; You could supply an anonymous (or lambda) function as part of the ﬁnd function, rather
;; than write a separate function:

(find "tiger" a-list (fn (x y) (> (length x) (length y))))


;; If you want your code to be readable, you'll probably move longer or more complex com-
;; parators out into their own separate - and documented - functions.
;; You can also use comparison functions with ref, ref-all, and replace.
;; A comparison function can be any function that takes two values and returns true or false.
;; For example, here's a function that returns true if y is greater than 6 and less than x. The
;; search is therefore for an element of the data list that is both smaller than the searched-for
;; number, which in this case is 15, and yet bigger than 6.

(define (is-between-and-greater-than x y)
    (and (> y 6) (< x y)))
    (find y a-list (fn (x y) (is-between-and-greater
     x y))))
     ;;-> 10

(set 'data '(31 23 -63 53 8 -6 -16 71 -124 29))
     (define (my-func x y)
             (and (> x y) (> y 6)))
(find 15 data my-func)
;-> 4 ; there's an 8 at index location 4

;; 4.3.9 Summary: compare and contrast
;; To summarize these contains functions, here they are in action:
;; 53 Lists
(set 'data
'("this" "is" "a" "list" "of" "strings" "not" "of" "integers"))

(find "of" data)
;-> 4; equality is default test
; index of first occurrence
(ref "of" data)
;-> (4); where is "of"?
; returns a list of indexes
(ref-all "of" data)
;-> ((4) (7)); list of address lists
(filter (fn (x) (= "of" x)) data)
;-> ("of" "of"); keep every of
(index (fn (x) (= "of" x)) data)
;-> (4 7); indexes of the of's
(match (* "of" * "of" *) data)
; three lists between the of's
;-> (("this" "is" "a" "list") ("strings" "not") ("integers"))
(member "of" data)
; and the rest
;-> ("of" "strings" "not" "of" "integers")
(count (list "of") data)
;-> (2)
; remember to use two lists
; returns list of counts

;; 4.4 Selecting items from lists

;; There are various functions for getting at the information stored in a list:
;; • ﬁrst gets the ﬁrst element
;; • rest gets all but the ﬁrst element
;; • last returns the last element
;; • nth gets the nth element
;; • select selects certain elements by index
;; • slice extracts a sublist
;; The ﬁrst and rest functions are more sensible names for the traditional car and cdr LISP
;; functions, which were based on the names of old computer hardware registers.


;; 4.4.1 Picking elements: nth, select, and slice
;; nth gets the nth element of a list:

(set 'phrase '("the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dog"))
(nth 1 phrase)
;-> "quick"
;; nth can also look inside nested lists, because it accepts more than one index number:

(set 'zoo
     '(("ape" 3)
      ("bat" 47)
      ("lion" 4)))
(nth '(2 1) zoo)
;-> 4


;; 4.4.2 Selecting elements: select, and slice
;; select returns a new list containing only the elements that satisfy a given condition:

;; If you want to pick a group of elements out of a list, you'll ﬁnd select useful. You can use
;; it in two diﬀerent forms. The ﬁrst form lets you supply a sequence of loose index numbers:

(set 'phrase '("the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dog"))

(select phrase 0 -2 3 4 -4 6 1 -1)
;-> ("the" "lazy" "fox" "jumped" "over" "the" "quick" "dog")
;; The second form lets you supply a function as the condition. In this case, we're picking
;; the second and third elements of the list:

(set 'zoo
     '(("ape" 3)
      ("bat" 47)
      ("lion" 4)))
      (select zoo (fn (x) (second x)) 1 2))
      ;-> (47 4)
      (select zoo (fn (x) (> (second x) 40)) 0
      1)
      ;-> (("ape" 3) ("lion" 4))
      (select zoo (fn (x) (> (second x) 40)) 0
      2)
      ;-> (("bat" 47) ("lion" 4))
      (select zoo (fn (x) (> (second x) 40)) 1
      1)
      ;-> ()
      (select zoo (fn (x) (> (second x) 40)) 1
      2)
      ;-> ()
      (select zoo (fn (x) (> (second x) 40)) 2
      1)
      ;-> ()
      (select zoo (fn (x) (> (second x) 40)) 2
      2)
      ;-> ()
      (select zoo (fn (x) (> (second x) 40)) 3
      1)
      ;-> ()
      
;; A positive number selects an element by counting forward from the beginning, and a negative
;; number selects by counting backwards from the end:


;; You can also supply a list of index numbers to select. For example, you can use the rand
;; function to generate a list of 20 random numbers between 0 and 8, and then use this list to
;; select elements from phrase at random:

(select phrase (rand 9 20))
;-> ("jumped" "lazy" "over" "brown" "jumped" "dog" "the" "dog" "dog"
; "quick" "the" "dog" "the" "dog" "the" "brown" "lazy" "lazy" "lazy" "quick")

;; 4.4.3 Summary: compare and contrast
;; To summarize these functions, here they are in action:
;; 53 Lists

(set 'phrase '("the" "quick" "brown" "fox" "jumped", "over" "the" "lazy" "dog"))
(nth 1 phrase)
;-> "quick"


;; 4.4 Selecting items from lists

;; There are various functions for getting at the information stored in a list:
;; • ﬁrst gets the ﬁrst element
;; • rest gets all but the ﬁrst element
;; • last returns the last element
;; • nth gets the nth element
;; The ﬁrst and rest functions are more sensible names for the traditional car and c
;; functions, which were based on the names of old computer hardware registers.

;; Notice the duplicates. If you had written this instead:
(randomize phrase)

;; there would be no duplicates: (randomize phrase) shuﬄes elements without duplicating
;; them.
;; slice lets you extract sections of a list. Supply it with the list, followed by one or two
;; numbers. The ﬁrst number is the start location. If you miss out the second number, the
;; rest of the list is returned. The second number, if positive, is the number of elements to
;; return.

(slice (explode "schwarzwalderkirschtorte") 7)
;-> ("w" "a" "l" "d" "e" "r" "k" "i" "r" "s" "c" "h" "t" "o" "r" "t" "e")
(slice (explode "schwarzwalderkirschtorte") 7 6)
;-> ("w" "a" "l" "d" "e" "r")


;; 4.4.3 Summary: compare and contrast
;; To summarize these functions, here they are in action:
;; 53 Lists
(set 'phrase '("the" "quick" "brown" "fox" "jumped", "over" "the" "lazy" "dog"))


;; If negative, the second number speciﬁes an element at the other end of the slice counting
;; backwards from the end of the list, -1 being the ﬁnal element:

(slice (explode "schwarzwalderkirschtorte") 19 -1)
;-> ("t" "o" "r" "t")

;; The cake delicious reaches as far as - but doesn't include - the element you specify.

;; 4.5 Implicit addressing

;; newLISP provides a faster and more eﬃcient way of selecting and slicing lists. Instead of
;; using a function, you can use index numbers and lists together. This technique is called
;; implicit addressing.

;; 4.5.1 Select elements using implicit addressing
;; As an alternative to using nth, put the list's symbol and an index number in a list, like
;; this:

(set 'r '("the" "cat" "sat" "on" "the" "math"))
(r 1)
; element index 1 of r
;-> "cat"
(nth 1 r)
;-> "cat"
; the equivalent using nth
(r 0)
;-> "the"
(r -1)
;-> "math"

;; If you have a nested list, you can supply a sequence of index numbers that identify the list
;; in the hierarchy:
(set 'zoo
     '(("ape" 3)
      ("bat" 47)
      ("lion" 4)))
(zoo 2 1)
;-> 4
(nth '(2 1) zoo)
;-> 4
; three sublists in a list
; the equivalent using nth

;; where the (2 1)ﬁrst ﬁnds element 2,("lion" 4), then ﬁnds element 1 (the second
;; one) of that sublist.

;; 4.5.2 Selecting a slice using implicit addressing

;; You can also use implicit addressing to get a slice of a list. This time, put one or two
;; numbers to deﬁne the slice, before the list's symbol, inside a list:

(set 'alphabet '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
"l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
(13 alphabet)
; start at 13, get the rest
;-> ("n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
(slice alphabet 13)
; equivalent using slice
;-> ("n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")
(3 7 alphabet)
; start at 3, get 7 elements
;-> ("d" "e" "f" "g" "h" "i" "j")

(slice alphabet 3 7)
; equivalent using slice
;-> ("d" "e" "f" "g" "h" "i" "j")

;; Earlier, we parsed the iTunes XML library:
(xml-type-tags nil nil nil nil)
    (silent
    (set 'itunes-data
    (xml-parse
    (read-file
     "/Users/me/Music/iTunes/iTunes Music Library.xml")
    (+ 1 2 4 8 16))))

    (let ((tracks (xml-select itunes-data 'track)))
    (let ((track-titles (mapcar (fn (track) (xml-get-content
    (xml-select track 'title))) tracks)))
    (print track-titles))))
    ;-> ("The Dark Side of the Moon" "A Day in the Life" "S
    