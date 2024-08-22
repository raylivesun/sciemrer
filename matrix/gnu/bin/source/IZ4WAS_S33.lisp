;; How are you going to add these numbers up, and then divide by the total, to ﬁnd the
;; average? Perhaps you think you could use add, which totals a list of ﬂoating-point numbers,
;; but you're not working interactively, so you can't edit the code to read like this:

(add 0.1 3.2 -1.2 1.2 -2.3 0.1 1.4 2.5 0.3)

;; Since we're holding the data in a symbol called data, we might try this:

(add data)

;; But we don't know what data is, so we can't do this:

(divide (add data) (length data))

;; Instead, we might try this:

(let ((total (reduce + data)))
  (divide total (length data)))


;; but no, this doesn't work, because add wants numbers to add, not a list. You could of
;; course do it the hard way and write a loop that works through each item in the list and
;; increases a running total each time:

(set 'total 0)
     (dolist (i data)
     (inc total i))
(println total)

;; This works ﬁne. But newLISP has a much more powerful solution, for this and many other
;; problems: you can treat functions as data and data as functions, so you can manipulate
;; functions as easily as you can manipulate your data. You can just 'introduce' add and the
;; data list to each other, and then stand back and let them get on with it.
;; There are two important functions for doing this: apply and map.

;; 6.1.1 apply
;; apply takes a function and a list, and makes them work together:

(apply add data)
;-> 5.3
(div (apply add data) (length data))
;-> 0.5888888889

;; and this produces the required result. Here we've treated the add function like any other
;; newLISP list, string, or number, using it as an argument to another function. You don't
;; need to quote it (although you can), because apply is already expecting the name of a
;; function.


;; 6.1.2 map
;; map is like apply, but it applies a function to each item in a list, instead of
;; applying a function to a list of arguments.

(map #'add data)
;-> (5.3 4.0 -1.2 2.2 -1
;-> 0.5888888889)


;; map is often a more readable and convenient way to apply a function to a list than
;; using apply. For example, instead of writing this:

(mapcar #'(lambda (x) (* x 2)) data)
;; you can write this:

(map '* 2 data)
;-> (1.0 6.4 -2.4 4.4 -2


;; 6.1.3 Other useful functions
;; There are many other useful functions in newLISP, and they're all built into the language
;; itself. Here are a few examples:

;; list: makes a new list from the arguments

(list 1 2 3 4 5)
;-> (1 2 3 4 5)


;; length: returns the number of items in a list

(length '(1 2 3 4 5))
;-> 5



;; nth: returns the nth item in a list

(nth 3 '(1 2 3 4 5))


;; cons: creates a new list by adding an item to the beginning of an existing list

(cons 6 '(1 2 3 4 5))
;-> (6 1 2 3 4 5)


;; 6.1.2 map

;; The other function that can make functions and lists work together is map, which applies
;; a function to each item of a list, one by one. For example, if you wanted to apply the ﬂoor
;; function to each element of the data list (to round them down to the nearest integer) you
;; could combine map, ﬂoor, and the data as follows:

(map #'floor data)


;; 6.1.3 Other useful functions
;; There are many other useful functions in newLISP, and they're all built into the language
;; itself. Here are a few examples:

;; list: makes a new list from the arguments

(list 1 2 3 4 5)
;-> (1 2 3 4 5)

;; 6.1.3 apply and map in more detail

;; Both apply and map let you treat functions as data. They have the same basic form:

(apply f l)
(map f l)

;; where f is the name of a function and l is a list. The idea is that you tell newLISP to process
;; a list using the function you specify.


;; 6.1.4 Applying functions to data
;; The simplest way to apply a function to data is to use apply:

(apply add '(1 2 3 4 5))
;-> 15

;; 6.1.5 Mapping functions to data
;; The map function is like apply, but it applies a function to each item in a list,
;; instead of applying a function to a list of arguments. For example, if you wanted to apply
;; the square function to each element of the data list (to square each number) you could
;; combine map, square, and the data as follows:

(map #'square data)
;-> (1 4 9 16 25)

;; 6.1.6 Other useful functions
;; There are many other useful functions in newLISP, and they're all built into the language
;; itself. Here are a few examples:

;; list: makes a new list from the arguments

(list 1 2 3 4 5)
;-> (1 2 3 4 5)

;; length: returns the number of items in a list

(length '(1 2 3 4 5))
;-> 5

;; The apply function uses all the elements in the list as arguments to the function, and
;; evaluates the result.

(apply reverse '("this is a string"))
;-> "gnirts a si siht"


;; Here, apply looks at the list, which in this case consists of a single string, and feeds the
;; elements to the function as arguments. The string gets reversed. Notice that you don't have
;; to quote the function but you do have to quote the list, because you don't want newLISP
;; to evaluate it before the designated function gets a chance to consume it.
;; The map function, on the other hand, works through the list, element by element, like a
;; sergeant major inspecting a row of soldiers, and applies the function to each element in turn,
;; using the element as the argument. However, map remembers the result of each evaluation
;; as it goes, collects them up, and returns them in a new list.
;; So map looks like a control-ﬂow word, a bit like dolist, whereas apply is a way of controlling
;; the newLISP list evaluation process from within your program, calling a function when and
;; where you want it called, not just as part of the normal evaluation process.
;; If we adapt the previous example for map, it gives a similar result, although the result is
;; a list, not a string:

(map reverse '("this is a string"))
;-> ("gnirts a si siht")

;; Because we've used a list with only one element, the result is almost identical to the apply
;; example, although notice that map returns a list whereas, in this example, apply returns
;; a string:

(apply reverse '("this is a string"))
;-> "gnirts a si siht"

;; The string has been extracted from the list, reversed, and then stored in another list created
;; by map.
;; In the next example:

(map reverse '("this" "is" "a" "list" "of" "strings"))
;-> ("siht" "si" "a" "tsil" "fo" "sgnirts")

;; you can clearly see that map has applied reverse to each element of the list in turn, and
;; returned a list of the resulting reversed strings.

;; 6.2 Write one in terms of the other?
;; To illustrate the relationship between these two functions, here is an attempt at deﬁning
;; map in terms of apply:

(define (my-map f l , r)
    ; declare a local variable r to hold the results
    (dolist (e l)
    (push (apply f (list e)) r -1)))

;; We're pushing the result of applying a function f to each list item to the end of a temporary
;; list, and then relying on push returning the list at the end, just as map would do. This
;; works, at least for simple expressions:

(my-map explode '("this is a string"))
;-> ("t" "h" "i" "s" " " "i" "s" " " "a" " " "s" "t" "r" "i" "n" "g")

(map explode '("this is a string"))
;-> (("t" "h" "i" "s" " " "i" "s" " " "a" " " "s" "t" "r" "i" "n" "g"))

;; This example illustrates why map is so useful. It's an easy way to transform all the
;; elements of a list without the hassle of working through them element by element using a
;; dolist expression.

;; 6.3 More tricks

;; Both map and apply have more tricks up their sleeves. map can traverse more than one
;; list at the same time. If you supply two or more lists, newLISP interleaves the elements of
;; each list together, starting with the ﬁrst elements of each list, and passes them as arguments
;; to the function:

(map append '("cats " "dogs " "birds ") '("miaow" "bark" "tweet"))
;-> ("cats miaow" "dogs bark" "birds tweet")

;; In this example, map takes two lists and uses append to interleave their elements,
;; producing a new list with the concatenated strings.

;; Here the ﬁrst element of each list is passed as a pair to append, followed by the second
;; element of each list, and so on.

;; This weaving together of strands is a bit like knitting with lists. Or like doing up a zip.
;; apply has a trick too. A third argument indicates how many of the preceding list's ar-
;; guments the function should use. So if a function takes two arguments, and you supply
;; three or more, apply comes back and makes another attempt, using the result of the ﬁrst
;; application and another argument. It continues eating its way through the list until all the
;; arguments are used up.

;; To see this in action, let's ﬁrst deﬁne a function that takes two arguments and compares
;; their lengths:

 (define (compare-lengths x y)
 (if (= (length x) (length y))
     'equal
     (if (> (length x) (length y))
         'longer
         'shorter)))


;; Now we can use apply to compare the lengths of the strings in the list:

(apply compare-lengths '("this is a string" "a much longer string"))
;-> 'longer


;; 6.4 More tricks

;; 6.4.1 Applying functions to data with more than one argument
;; If a function takes more than one argument, apply can be used to apply that function to
;; each element of a list. For example, if you wanted to apply the square function to each
;; element of the data list (to square each number), you could combine apply, square,
;; and the data as follows:

(apply map square '(1 2 3 4 5))
;-> (1 4 9 16 25)


;; 6.4.2 Applying functions to data with more than one argument
;; The apply function uses all the elements in the list as arguments to the function, and
;; evaluates the result.

(apply reverse '("this is a string"))
;-> "gnirts a si siht"


;; 6.4.3 Applying functions to data with more than one argument
;; The map function is like apply, but it applies a function to each item in a list,
;; instead of applying a function to a list of arguments. For example, if you wanted to apply
;; the square function to each element of the data list (to square each number) you could
;; combine map, square, and the data as follows:

(map #'square data)
;-> (1 4 9 16 25)

(define (longest s1 s2)
        (println s1 " is longest so far, is " s2 " longer?") ; feedback
            (if (>= (length s1) (length s2))
            ; compare
            s1
            s2))


;; Now let's test this function:

(longest "hello" "world")
;-> "hello is longest so far, is world longer?"
;-> "world"

;; Now you can apply this function to a list of strings, using the third argument to tell apply
;; to use up the arguments two strings at a time:
(apply longest '("green" "purple" "violet" "yellow" "orange"
"black" "white" "pink" "red" "turquoise" "cerise" "scarlet"
"lilac" "grey" "blue") 2)


;; It's like walking along the beach and ﬁnding a pebble, and holding on to it until an even
;; better one turns up.
;; apply also gives you a way of working through a list and applying a function to each pair
;; of items:

(apply (fn (x y)
       (println {x is } x {, y is } y)) (sequence 0 10) 2)

;; What's happening here is that the value returned by the println function is the second
;; member of the pair, and this becomes the value of the ﬁrst element of the next pair.

;; 6.4.4 Applying functions to data with more than one argument

;; 6.4.5 Applying functions to data with more than one argument

;; 6.4.6 Applying functions to data with more than one argument

;; 6.4.7 Applying functions to data with more than one argument

;; 6.4.8 Applying functions to data with more than one argument

;; 6.5 Conclusion

;; 6.5.1 Conclusion


;; 6.5.2 Conclusion

;; 6.5.3 Conclusion

;; 6.5.4 Conclusion

;; 6.5.5 Conclusion

;; 6.5.6 Conclusion

;; 6.5.7 Conclusion

;; 6.5.8 Conclusion

;; 6.5.9 Conclusion

;; 6.5.10 Conclusion

;; 6.4 Lispiness

;; This thing about passing around the names of functions as if they were bits of data is very
;; characteristic of newLISP, and it's very useful. You will ﬁnd many uses for it, sometimes
;; using functions that you don't think will be useful with map. Here, for example, is set
;; working hard under the control of map:

(map set '(a b) '(1 2))
;-> a is 1, b is 2
(map set '(a b) (list b a))
;-> a is 2, b is 1

;; This construction gives you another way to assign values to symbols in parallel, rather than
;; sequentially. (You can use swap as well.)
;; Some uses of map are simple:

(map char (explode "hi there"))
;-> (104 105 32 116 104 101 114 101)
(map (fn (h) (format "%02x" h)) (sequence 0 15))


;; 6.4.1 Applying functions to data with more than one argument

;; 6.4.2 Applying functions to data with more than one argument

;; 6.4.3 Applying functions to data with more than one argument

;; 6.4.4 Applying functions to data with more than one argument

;; 6.4.5 Applying functions to data with more than one argument

;; 6.4.6 Applying functions to data with more than one argument

;; 6.4.7 Applying functions to data with more than one argument

;; 6.4.8 Applying functions to data with more than one argument

;; 6.4.9 Applying functions to data with more than one argument

;; 6.4.10 Applying functions to data with more than one argument

;; Others can become quite complex. For example, given a string of data in this form, stored
;; in a symbol image-data:

("/Users/me/graphics/file1.jpg" "
pixelHeight: 978" "
pixelWidth: 1181")


;; the two numbers can be extracted with:

(map set '(height width) (map int (map last (map parse (rest image-data)))))
 
;; 6.5 currying

;; Some of the built-in newLISP functions do things with other functions. An example is
;; curry, which creates a copy of a two-argument function and creates a single-argument
;; version with a pre-determined ﬁrst argument. So if a function f1 was often called like this:

(f1 arg1 arg2)


;; the curried version of f1 would be:

(curry f1 arg1)


;; 6.5.1 currying


;; you can use curry to make a new function f2 that has a ready-to-use built-in arg1:

(set 'f2 (curry f1 arg1))

;; now you can forget about that ﬁrst argument, and just supply the second one to f2:

(f2 arg2)

;; Why is this useful? Consider the dup function, which often gets used to insert multiple
;; blank spaces:

(dup { } 10)


;; 6.5.2 currying

;; 6.5.3 currying

;; 6.5.4 currying

;; 6.5.5 currying

;; 6.5.6 currying

;; 6.5.7 currying

;; 6.5.8 currying

;; 6.5.9 currying

;; 6.5.10 currying

;; Using curry, you can create a new function called, say, blank, that's a special version of
;; dup that always gets called with a blank space as the string:

(set 'blank (curry dup { }))

;; now you can use blank to insert multiple blank spaces in a single line of code:

(print (blank " " 10))

;; 6.6 Summary

;; 6.6.1 Summary

;; 6.6.2 Summary

;; 6.6.3 Summary

;; 6.6.4 Summary

;; 6.6.5 Summary

;; 6.6.6 Summary

;; 6.6.7 Summary

;; 6.6.8 Summary

;; 6.6.9 Summary

;; 6.6.10 Summary


(blank 10)
;->
; 10 spaces
;; curry can be useful for creating temporary or anonymous functions with map:
(map (curry pow 2) (sequence 1 10))
;-> (2 4 8 16 32 64 128 256 512 1024)
(map (fn (x) (pow 2 x)) (sequence 1 10)) ; equivalent
;-> (2 4 8 16 32 64 128 256 512 1024)

;; But avoid using curry on destructive1 functions like inc, for example:
(setq a-list-of-pairs (sequence 2 10 2))
;-> (2 4 6 8 10)
(map (curry inc 3) a-list-of-pairs) ;-> you would expect (5 7 9 11 13), instead you get
;-> (5 9 15 23 33)
; one proper way to get every number incremented by 3 would be
(map (curry + 3) a-list-of-pairs)
;-> (5 7 9 11 13)
; or if you insist in using inc, then provide a copy of the increment so the
;; reference inc gets doesn't mess up things
(map (curry inc (copy 3)) a-list-of-pairs)
;-> (5 7 9 11 13)

;; 7 Introducing contexts

;; We all like to organize our stuﬀ into separate areas or compartments. Chefs keep their ﬁsh,
;; meat, and dessert areas separate, electronics engineers keep their power supplies away from
;; their radio frequency and audio stages, and newLISP programmers use contexts to organize
;; their code.

;; 7.1 What is a context?

;; A newLISP context provides a named container for symbols. Symbols in diﬀerent contexts
;; can have the same name without clashing. So, for example, in one context I can deﬁne the
;; symbol called meaning-of-life to have the value 42, but, in another context, the identically-
;; named symbol could have the value dna-propagation, and, in yet another, worship-of-deity.
;; Unless you speciﬁcally choose to create and/or switch contexts, all your newLISP work is
;; carried out in the default context, called MAIN. So far in this document, when new symbols
;; have been created, they've been added to the MAIN context.
;; Contexts are very versatile - you can use them for dictionaries, or software objects, or
;; super-functions, depending on the task in hand.


;; 7.2 Creating a new context


;; 7.2.1 Creating a new context

;; 7.2.2 Creating a new context

;; 7.2.3 Creating a new context

;; 7.2.4 Creating a new context

;; 7.3 Switching between contexts

;; 7.3.1 Switching between contexts

;; 7.3.2 Switching between contexts

;; 7.3.3 Switching between contexts

;; 7.3.4 Switching between contexts

;; 7.4 Context-bound symbols

;; 7.4.1 Context-bound symbols

;; 7.4.2 Context-bound symbols

;; 7.4.3 Context-bound symbols

;; 7.4.4 Context-bound symbols

;; 7.5 Context-bound functions

;; 7.5.1 Context-bound functions

;; 7.5.2 Context-bound functions

;; 7.5.3 Context-bound functions

;; 7.5.4 Context-bound functions

;; 7.6 Context-bound macros

;; 7.6.1 Context-bound macros

;; 7.6.2 Context-bound macros

;; 7.6.3 Context-bound macros

;; 7.6.4 Context-bound macros

;; 7.7 Summary

;; 7.7.1 Summary

;; 7.7.2 Summary

;; 7.7.3 Summary

;; 7.7.4 Summary

;; 7.7.5 Summary

;; 7.7.6 Summary

;; 7.7.7 Summary

;; 7.7.8 Summary

;; 7.7.9 Summary

;; 7.7.10 Summary

;; 8 More advanced topics

;; 8.1 More advanced topics

;; 8.1.1 More advanced topics

;; 8.1.2 More advanced topics

;; 8.1.3 More advanced topics

;; 8.1.4 More advanced topics

;; 8.2 More advanced topics

;; 8.2.1 More advanced topics

;; 8.2.2 More advanced topics

;; 8.2.3 More advanced topics

;; 8.2.4 More advanced topics

;; 8.3 More advanced topics

;; 8.3.1 More advanced topics

;; 8.3.2 More advanced topics

;; 8.3.3 More advanced topics

;; 8.3.4 More advanced topics

;; 8.4 More advanced topics

;; 8.4.1 More advanced topics

;; 8.4.2 More advanced topics

;; 8.4.3 More advanced topics

;; 8.4.4 More advanced topics

;; 8.5 More advanced topics

;; 8.5.1 More advanced topics

;; 8.5.2 More advanced topics

;; 8.5.3 More advanced topics

;; 8.5.4 More advanced topics

;; 8.6 More advanced topics

;; 8.6.1 More advanced topics

;; 8.6.2 More advanced topics

;; 8.6.3 More advanced topics

;; 8.6.4 More advanced topics

;; 8.7 More advanced topics

;; 8.7.1 More advanced topics

;; 8.7.2 More advanced topics

;; 8.7.3 More advanced topics

;; 8.7.4 More advanced topics

;; 8.8 More advanced topics

;; 8.8.1 More advanced topics

;; 8.8.2 More advanced topics

;; 8.8.3 More advanced topics

;; 8.8.4 More advanced topics

;; 8.9 More advanced topics

;; 8.9.1 More advanced topics

;; 8.9.2 More advanced topics

;; 8.9.3 More advanced topics

;; 8.9.4 More advanced topics

;; 8.10 More advanced topics

;; 8.10.1 More advanced topics

;; 8.10.2 More advanced topics

;; 8.10.3 More advanced topics

;; 8.10.4 More advanced topics

;; 8.11 More advanced topics

;; 8.11.1 More advanced topics

;; 8.11.2 More advanced topics

;; 8.11.3 More advanced topics

;; 8.11.4 More advanced topics

;; 8.12 More advanced topics

;; 8.12.1 More advanced topics

;; 8.12.2 More advanced topics

;; 8.12.3 More advanced topics

;; 8.12.4 More advanced topics

;; 9 Conclusion

;; 9.1 Conclusion

;; 9.2 Conclusion

;; 9.3 Conclusion

;; 9.4 Conclusion

;; 9.5 Conclusion

;; 9.6 Conclusion

;; 9.7 Conclusion

;; 9.8 Conclusion

;; 9.9 Conclusion

;; 9.10 Conclusion

;; 9.11 Conclusion

;; 9.12 Conclusion

;; 9.13 Conclusion

;; 9.14 Conclusion

;; 9.15 Conclusion

;; 9.16 Conclusion

;; 9.17 Conclusion

;; 9.18 Conclusion

;; 9.19 Conclusion

;; 9.20 Conclusion

;; 9.21 Conclusion

;; 9.22 Conclusion

;; 9.23 Conclusion

;; 9.24 Conclusion

;; 9.25 Conclusion

;; 9.26 Conclusion

;; 9.27 Conclusion

;; 9.28 Conclusion

;; 9.29 Conclusion

;; 9.30 Conclusion

;; 9.31 Conclusion

;; 9.32 Conclusion

;; 9.33 Conclusion

;; 9.34 Conclusion

;; 9.35 Conclusion

;; 9.36 Conclusion


;; 9.37 Conclusion

;; 9.38 Conclusion

;; 9.39 Conclusion

;; 9.40 Conclusion

;; 9.41 Conclusion

;; 9.42 Conclusion

;; 9.43 Conclusion

;; 9.44 Conclusion

;; 9.45 Conclusion

;; 9.46 Conclusion

;; 9.47 Conclusion

;; 9.48 Conclusion

;; 9.49 Conclusion

;; 9.50 Conclusion

;; 9.51 Conclusion

;; 9.52 Conclusion

;; 9.53 Conclusion

;; 9.54 Conclusion

;; 9.55 Conclusion

;; 9.56 Conclusion

;; 9.57 Conclusion

;; 9.58 Conclusion

;; 9.59 Conclusion

;; 9.60 Conclusion

;; 9.61 Conclusion

;; 9.62 Conclusion

;; 9.63 Conclusion

;; 9.64 Conclusion

;; 9.65 Conclusion

;; 9.66 Conclusion

;; 9.67 Conclusion

;; 9.68 Conclusion

;; 9.69 Conclusion

;; 9.70 Conclusion

;; 9.71 Conclusion

;; 9.72 Conclusion

;; 9.73 Conclusion

;; 9.74 Conclusion

;; 9.75 Conclusion

;; 9.76 Conclusion

;; 9.77 Conclusion

;; 9.78 Conclusion

;; 9.79 Conclusion

;; 9.80 Conclusion

;; 9.81 Conclusion

;; 9.82 Conclusion

;; 9.83 Conclusion

;; 9.84 Conclusion

;; 9.85 Conclusion

;; 9.86 Conclusion

;; 9.87 Conclusion

;; 9.88 Conclusion

;; 9.89 Conclusion

;; 9.90 Conclusion

;; 9.91 Conclusion

;; 9.92 Conclusion

;; 9.93 Conclusion

;; 9.94 Conclusion

;; 9.95 Conclusion

;; 9.96 Conclusion

;; 9.97 Conclusion

;; 9.98 Conclusion

;; 9.99 Conclusion

;; 9.100 Conclusion

;; 9.101 Conclusion

;; 9.102 Conclusion

;; 9.103 Conclusion

;; 9.104 Conclusion

;; 9.105 Conclusion

;; 9.106 Conclusion

;; 9.107 Conclusion

;; 9.108 Conclusion

;; 9.109 Conclusion

;; 9.110 Conclusion

;; 9.111 Conclusion

;; 9.112 Conclusion

;; 9.113 Conclusion

;; 9.114 Conclusion

;; 9.115 Conclusion

;; 9.116 Conclusion

;; 9.117 Conclusion

;; 9.118 Conclusion

;; 9.119 Conclusion

;; 9.120 Conclusion

;; 9.121 Conclusion

;; 9.122 Conclusion

;; 9.123 Conclusion

;; 9.124 Conclusion

;; 9.125 Conclusion

;; 9.126 Conclusion

;; 9.127 Conclusion

;; 9.128 Conclusion

;; 9.129 Conclusion

;; 9.130 Conclusion

;; 9.131 Conclusion

;; 9.132 Conclusion

;; 9.133 Conclusion

;; 9.134 Conclusion

;; 9.135 Conclusion

;; 9.136 Conclusion

;; 9.137 Conclusion

;; 9.138 Conclusion

;; 9.139 Conclusion

;; 9.140 Conclusion

;; 9.141 Conclusion










