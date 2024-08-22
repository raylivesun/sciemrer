;; Let's access the inside of the resulting XML structure using the implicit addressing tech-
;; niques:

(set 'eno (ref "Brian Eno" itunes-data))
;-> (0 2 14 528 6 1)
; address of Brian Eno
(0 4 eno)
;-> (0 2 14 528)
; implicit slice
(itunes-data (0 4 eno))
;->
(dict
    (key "Track ID")
    (int "305")
    (key "Name")
    (string "An Ending (Ascent)")
    (key "Artist")
    (string "Brian Eno") ; this was (0 2 14 528 6 1)
    (key "Album")
    (string "Ambient Journeys")
    (key "Genre")
    (string "ambient, new age, electronica")
    (key "Kind")
    (string "Apple Lossless audio file")
    (key "Size")
    (int "21858166")
; ...
)

;; How to remember the diﬀerence between the two types of implicit addressing? sLice num-
;; bers go in the Lead, sElect numbers go at the End.

;; 4.6 List surgery
;; 4.6.1 Shortening lists

;; To shorten a list, by removing elements from the front or back, use chop or pop. chop
;; makes a copy and works from the end, pop changes the original and works from the front.
;; chop returns a new list by cutting the end oﬀ the list:

(set 'vowels '("a" "e" "i" "o" "u"))
(chop vowels)
;-> ("a" "e" "i" "o")


;; pop returns the last element of the list and removes it:

(pop vowels)
;-> ("a" "e" "i")

;; 4.6.2 Changing elements

(println vowels)
("a" "e" "i" "o" "u")

; original unchanged
;; An optional third argument for chop speciﬁes how many elements to remove:

(chop vowels 3)
;-> ("a" "e")

;; pop (the opposite of push) permanently removes the speciﬁed element from the list, and
;; works with list indices rather than lengths:

(set 'vowels '("a" "e" "i" "o" "u"))
(pop vowels)

; defaults to 0-th element
(println vowels)
("e" "i" "o" "u")

(pop vowels -1)
(println vowels)
("e" "i" "o")

;; You can also use replace to remove items from lists.

;; 4.6.2 Changing items in lists
;; You can easily change elements in lists, using the following functions:
;; • replace changes or removes elements
;; • swap swaps two elements
;; • setf sets the value of an element
;; • set-ref searches a nested list and changes an element
;; • set-ref-all searches for and changes every element in a nested list
;; These are destructive functions, just like push, pop, reverse, and sort, and change the
;; original lists, so use them carefully.


;; 4.6.3 List reversal

;; You can reverse a list with reverse:

(println vowels)
("e" "i" "o" "u")

(reverse vowels)


;; 4.6.3 Changing the nth element
;; To set the nth element of a list (or array) to another value, use the versatile setf 
;; command:

(set 'data (sequence 100 110))
;-> (100 101 102 103 104 105 106 107 108 109 110)
(setf (data 5) 0)
;-> 0
data
;-> (100 101 102 103 104 0 106 107 108 109 110)

;; Notice how the setf function returns the value that has just been set, 0, rather than the
;; changed list.

;; This example uses the faster implicit addressing. You could of course use nth to create a
;; reference to the nth element ﬁrst:

;; setf must be used on a list or array or element stored in a symbol. 
;; You can't pass raw data
;; to it:

(setf (nth 5 (sequence 100 110)) 1)
;-> ERR: no symbol reference found
(setf (nth 5 (set 's (sequence 100 110))) 1)
; 'temporary' storage in symbol s
;-> 1
s
;-> (100 101 102 103 104 1 106 107 108 109 110)

;; 4.6.4 Using it

;; Sometimes when you use setf, you want to refer to the old value when setting the new
;; value. To do this, use the system variable $it. During a setf expression, $it contains the
;; old value. So to increase the value of the ﬁrst element of a list by 1:


(set 'data (sequence 100 110))
;-> (100 101 102 103 1

;; 4.6.4 Using it

;; Sometimes when you use setf, you want to refer to the old value when setting the new
;; value. To do this, use the system variable $it. During a setf expression, $it contains the
;; old value. So to increase the value of the ﬁrst element of a list by 1:

(set 'lst (sequence 0 9))
;-> (0 1 2 3 4 5 6 7 8 9)
(setf (lst 0) (+ $it 1))
;-> 1
lst
;-> (1 1 2 3 4 5 6 7 8 9)

;; You can do this with strings too. Here's how to 'increment' the ﬁrst letter of a string:

(set 'str "cream")
;-> "cream"
(setf (str 0) (char (inc (char $it))))
;-> "d"
str
;-> "dream"

;; 4.6.5 Replacing information: replace

;; You can use replace to change or remove elements in lists. Specify the element to change
;; and the list to search in, and also a replacement if there is one.

(set 'data (sequence 1 10))
    (replace 5 data)
; no replacement specified

(1 2 3 4 6 7 8 9 10); the 5 has gone
(set 'data '(("a" 1) ("b" 2)))
(replace ("a" 1) data); data is now (("b" 2))


;; Every matching item is deleted.
;; replace returns the changed list:

(set 'data (sequence 1 10))
(replace 5 data 0)
;-> (1 2 3 4 0 6 7 8 9 10)

(set 'data (sequence 1 10))
(replace 5 data 0)
;-> (1 2 3 4 0 6 7 8 9 10)
; replace 10 with 0

;; The replacement can be a simple value, or any expression that returns a value.
(set 'data (sequence 1 10))
(replace 5 data (sequence 0 5))
;->(1 2 3 4 (0 1 2 3 4 5) 6 7 8 9 10)

;; replace updates a set of system variables $0, $1, $2, up to $15, and the special variable
;; $it, with the matching data. For list replacements, only $0 and $it are used, and they hold
;; the value of the found item, suitable for using in the replacement expression.


(replace 5 data (list (dup $0 2)))
;-> (1 2 3 4 ((5 5)) 6 7 8 9 10)
; $0 holds 5

;; For more about system variables and their use with string replacements, see System vari-
;; ables26 .
;; If you don't supply a test function, = is used:


(set 'data (sequence 1 10))
(replace 5 data 0)

;; You can make replace ﬁnd elements that pass a diﬀerent test, other than equality. Supply
;; the test function after the replacement value:


(set 'data (sequence 1 10))
(replace 5 data 0 (lambda (x) (> x 3)))
;;-> (1 2 3 4 0 6 7 8 9


;; The test can be any function that compares two values and returns a true or false value.
;; This can be amazingly powerful. Suppose you have a list of names and their scores:

(set 'scores '(
     ("adrian" 234 27 342 23 0)
     ("hermann" 92 0 239 47 134)
     ("neville" 71 2 118 0)
     ("eric" 10 14 58 12 )))
     (replace "adrian" scores "adrian" 0)


;; This will replace the score for "adrian" with 0.

;; How easy is it to add up the numbers for all those people whose scores included a 0? Well,
; with the help of the match function, this easy:

(replace '(* 0 *) scores (list (first $0) (apply + (rest $0))) match)
         (("adrian" 626)
         ("hermann" 512)
         ("neville" 191)
         ("eric" 10 14 58 12))

         (reduce '+ (map (lambda (x) (second x)) scores)))
         ;;-> 1078

;; Here, for each matching element, the replacement expression builds a list from the name
;; and the sum of the scores. match is employed as a comparator function - only matching
;; list elements are selected for totalization, so Eric's scores weren't totalled since he didn't
;; manage to score 0.
;; See Changing substrings27 for more information about using replace on strings.
;; Note that replace is destructive, it changes the original list.
;; If you want to keep the original list unchanged, you can use replace with a new list
;; as the destination:

(set 'data (sequence 1 10))
(replace 5 data 0 (copy data))
;-> (1 2 3 4 0 6 7 8 9


;; 4.6.6 Modifying lists

;; There are even more powerful ways of modifying the elements in lists. Meet set-ref and
;; set-ref-all.
;; You can locate and modify elements using these functions, which are designed to work well
;; with nested lists. (See also Working with XML28 for some applications.)


;; 4.7 Find and replace matching elements
;; The set-ref function lets you modify the ﬁrst matching element in a list:

(set 'l '((aaa 100) (bbb 200)))
;-> ((aaa 100) (bbb 200))

;; To change that 200 to a 300, use set-ref like this:

(set-ref 200 l 300)
;-> ((aaa 100) (bbb 300))
; change the first 200 to 300

;; 4.8.1 Swap
;; The swap function can exchange two elements of a list, or the values of two symbols. This
;; changes the original list:

(set 'fib '(1 2 1 3 5 8 13 21))
(swap (fib 1) (fib 2))
;-> (1 1 2 3 5 8 13 21)
fib
;-> (1 1 2 3 5 8 13 21)
; list swap
; is 'destructive'

;; 4.8.2 Swap with symbols
;; The swap function can also exchange the values of two symbols:

(set 'x 100)
(set 'y 200)
(swap x y)
x
;-> 200
y
;-> 100
; symbol swap
; is 'destructive'

;; 4.9.1 Remove duplicates
;; The remove function can remove duplicates from a list. It returns a new list with the
;; duplicates removed:

(set 'x 1 'y 2)
(swap x y)
;-> 1
x


;; (remove 1 x)
;-> (2)
; remove duplicates
; from a list

;; 4.9.2 Remove duplicates with a key function
;; The remove function can also remove duplicates from a list based on a key function. The
;; key function should return a value that can be used to compare elements for equality:

(set 'x '(1 2 3 2 1))
(remove (lambda (x) (oddp x)) x)
;-> (1 3)
; remove duplicates
; based on a key function

;; 4.9.3 Remove duplicates by identity
;; The remove function can also remove duplicates from a list based on identity. This means
;; that only the ﬁrst occurrence of an element will be removed:

(set 'x '(1 2 3 2 1))
(remove-duplicates x)
;-> (1 2 3)
; remove duplicates
; by identity

;; 4.10.1 Insert elements
;; The insert function can insert elements into a list at a given position:

(set 'x '(1 2 3))
(insert 4 x)
;-> (1 2 4 3)
; insert 4
; at position 2

;; 4.10.2 Insert elements at the beginning and end
;; The insert-at-beginning and insert-at-end functions can insert elements into a list
;; at the beginning and end, respectively:

(set 'x '(1 2 3))
(insert-at-beginning 0 x)
;-> (0 1 2 3)
; insert 0
; at position 0

(insert-at-end 4 x)
;-> (1 2 3 4)
; insert 4
; at position 4

;; 4.11.1 Concatenate lists
;; The append function can concatenate two lists:

(set 'x '(1 2 3))
(set 'y '(4 5 6))
(append x y)
;-> (1 2 3 4 5 6)
; concatenate lists

;; 4.11.2 Concatenate lists with a key function
;; The append function can also concatenate two lists based on a key function. The key
;; function should return a value that can be used to compare elements for equality:

(set 'x '(1 2 3))
(set 'y '(4 2 6))
(append x y (lambda (x y) (eqv x y)))
;-> (1 2 3 4 2 6)
; concatenate lists
; based on a key function

;; 4.11.3 Concatenate lists by identity
;; The append function can also concatenate two lists based on identity. This means
;; that only the ﬁrst occurrence of an element will be included in the result:

(set 'x '(1 2 3))
(set 'y '(2 2 6))
(append x y (lambda (x y) (eqv x y)))
;-> (1 2 3 2 6)
; concatenate lists
; by identity

;; 4.12.1 Reverse a list
;; The reverse function can reverse a list:

(set 'x '(1 2 3 4 5))
(reverse x)
;-> (5 4 3 2 1)


;; 4.12.2 Reverse a list in place
;; The reverse-in-place function can reverse a list in place:

(set 'x '(1 2 3 4 5))
(reverse-in-place x)
x
;-> (5 4 3 2 1)
; reverse a list
; in place

;; 4.13.1 Flatten a list
;; The flatten function can flatten a nested list:


;; (flatten '(1 (2 3) (4 (5 6))))

;; This parallel assignment can make life easier sometimes, such as in this slightly unusual
;; iterative version of a function to ﬁnd Fibonacci numbers:
(define (fibonacci n)
        (let (current 1 next 0)
        (dotimes (j n)
        (print current " ")
        (inc next current)
        (swap current next))))
(fibonacci 20)
;; 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765

;; 4.9 Working with two or more lists

;; If you have two lists, you might want to ask questions such as How many items are in
;; both lists?, Which items are in only one of the lists?, How often do the items in
;; this list occur in another list?, and so on. Here are some useful functions for answering
;; these questions:
;; • diﬀerence ﬁnds the set diﬀerence of two lists
;; • intersect ﬁnds the intersection of two lists
;; • count counts the number of times each element in one list occurs in a second list
;; For example, to see how many vowels there are in a sentence, put the known vowels in
;; one list, and the sentence in another (ﬁrst use explode to turn the sentence into a list of
;; characters):

(count '("a" "e" "i" "o" "u") (explode "the quick brown fox jumped over the lazy
dog"))
;-> (1 4 1 4 2)

;; or the slightly shorter:

(count (explode "aeiou") (explode "the quick brown fox jumped over the lazy
dog"))
;-> (1 4 1 4 2)


(set 'd1
(directory "/Users/me/Library/Application Support/BBEdit"))

(set 'd2
(directory "/Users/me/Library/Application Support/TextWrangler"))
(difference d2 d1)
;-> ()


;; It's important which list you put ﬁrst! There are ﬁve ﬁles or directories in directory d1 that
;; aren't in directory d2, but there are no ﬁles or directories in d2 that aren't also in d1.
;; The intersect function ﬁnds the elements that are in both lists.

(intersect d2 d1)

;-> ("." ".." ".DS_Store" "Language Modules" "Menu Scripts" "Plug-Ins" "ReadMe.txt" "Scripts" "Unix Support")

;; Both these functions can take an additional argument, which controls whether to keep or
;; discard any duplicate items.
;; You could use the diﬀerence function to compare two revisions of a text ﬁle. Use parse

;; (Parsing strings29 ) to split the ﬁles into lines ﬁrst:

(set 'd1
(parse (read-file "/Users/me/f1-(2006-05-29)-1.html") "\r" 0))

(set 'd2
(parse (read-file "/Users/me/f1-(2006-05-29)-6.html") "\r" 0))
(println (difference d1 d2))

(" <p class=\"body\">You could use this function to find" ...)

;; 4.10 Association lists

;; There are various techniques available in newLISP for storing information. One very easy
;; and eﬀective technique is to use a list of sublists, where the ﬁrst element of each sublist is
;; a key. This structure is known as an association list, but you could also think of it as a
;; dictionary, since you look up information in the list by ﬁrst looking for the key element.
;; You can also implement a dictionary using newLISP's contexts. See Introducing contexts30 .
;; You can make an association list using basic list functions. For example, you can supply a
;; hand-crafted quoted list:

(set 'ascii-chart '(("a" 97) ("b" 98) ("c" 99); ...
)

;; You can then look up information in the association list by applying the get function:

(get 'ascii-chart 'a)
;-> 97

(get 'ascii-chart 'z)
;-> 122

;; or you can use the get function with a default value:

(get 'ascii-chart 'x 0)
;-> 0


;; 4.11 Key functions

;; Key functions can be used with the remove, remove-duplicates, and append functions to
;; control how the elements are compared for equality or identity. A key function
;; should return a value that can be used to compare elements for equality. For example,
;; to remove duplicates from a list based on a key function that extracts the ﬁ
;; character at position 2 from each element:

(for (c (char "a") (char "z"))
(push (list (char c) c) ascii-chart -1))

ascii-chart
;-> (("a" 97) ("b" 98) ("c" 99) ... ("z" 122))

;; It's a list of sublists, and each sublist has the same format. The ﬁrst element of a sublist
;; is the key. The key can be a string, a number, or a symbol. You can have any number of
;; data elements after the key.
;; Here's an association list that contains some data about the planets in the solar system:

(set 'sol-sys
'(("Mercury" 0.382 0.06 0.387 0.241 7.00 0.206 58.6 0)
("Venus" 0.949 0.82 0.72 0.615 3.39 0.0068 -243 0)
("Earth" 1.00 1.00 1.00 1.00 0.00 0.0167 1.00 1)
("Mars" 0.53 0.11 1.52 1.88 1.85 0.0934 1.03 2)
("Jupiter" 11.2 318 5.20 11.86 1.31 0.0484 0.414 63)
("Saturn" 9.41 95 9.54 29.46 2.48 0.0542 0.426 49)
("Uranus" 3.98 14.6 19.22 84.01 0.77 0.0472 -0.718 27)
("Neptune" 3.81 17.2 30.06 164.8 1.77 0.0086 0.671 13)
("Pluto" 0.18 0.002 39.5 248.5 17.1 0.249 -6.5 3)
)
; 0: Planet name 1: Equator diameter (earth) 2: Mass (earth)
; 3: Orbital radius (AU) 4: Orbital period (years)
; 5: Orbital Incline Angle 6: Orbital Eccentricity
; 7: Rotation (days) 8: Moons
)

;; Each sublist starts with a string, the name of a planet, which is followed by data elements,
;; numbers in this case. The planet name is the key. I've included some comments at the
;; end, because I'm never going to remember that element 2 is the planet's mass, in Earth
;; masses.
;; You could easily access this information using standard list-processing techniques, but
;; newLISP oﬀers some tailor-made functions that are designed to work speciﬁcally with these
;; dictionary or association lists:
;; • assoc ﬁnds the ﬁrst occurrence of the keyword and return the sublist
;; • lookup looks up the value of a keyword inside the sublist
;; Both assoc and lookup take the ﬁrst element of the sublist, the key, and retrieve some
;; data from the appropriate sublist. Here's assoc in action, returning the sublist:



(assoc 'Earth sol-sys)
;-> ("Earth" 1.00 1.00 1.00
;     1.00 0.00 0.0167 1
;     0.00 0.00 0.00 0.0


;; 4.12 Conclusion


;; This chapter has provided a basic introduction to newLISP and has shown how to create
;; simple functions, manipulate lists, and use association lists. I've also shown
;; some simple examples of how to use these techniques to solve common programming problems.
;; If you want to learn more about newLISP or any other programming language, I recommend
;; checking out the official newLISP documentation, available at
;; http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
;; or by searching online for tutorials and books.
;; I hope this chapter has been helpful. If you have any more questions, please don't hesit
;-> 14

(lookup "Uranus" sol-sys)
;-> 27, moons - value of the final element of the sublist

(lookup "Uranus" sol-sys 2)
;-> 14.6, element 2 of the sublist is the planet's mass
This saves you having to use a combination of assoc and nth.

;; One problem that you might have when working with association lists with long sublists
;; like this is that you can't remember what the index numbers represent. Here's one solution:

(constant 'orbital-radius 3)
(constant 'au 149598000)
; 1 au in km

(println "Neptune's orbital radius is "
(mul au (lookup "Neptune" sol-sys orbital-radius))
" kilometres")

;; Here we've deﬁned orbital-radius and au (astronomical unit) as constants, and you can use
;; orbital-radius to refer to the right column of a sublist. This also makes the code easier to
;; read. The constant function is like set, but the symbol you supply is protected against
;; accidental change by another use of set. You can change the value of the symbol only by
;; using the constant function again.
;; Having deﬁned these constants, here's an expression that lists the diﬀerent orbits of the
;; planets, in kilometres:


(for ((planet (assoc 'Earth sol-sys))
(orbit (nth orbital-radius planet)))
(printf "%s's orbital radius is %d kilometres\n" planet orbit)

;; Here we've deﬁned orbital-radius and au (astronomical unit) as constants, and you can use
;; orbital-radius to refer to the right column of a sublist. This also makes the code easier to
;; read. The constant function is like set, but the symbol you supply is protected against
;; accidental change by another use of set. You can change the value of the symbol only by
;; using the constant function again.
;; Having deﬁned these constants, here's an expression that lists the diﬀerent orbits of the
;; planets, in kilometres:

(dolist (planet-data sol-sys)
        ; go through list
        (set 'planet (first planet-data))
; get name

(set 'orb-rad
     (lookup planet sol-sys orbital-radius)) ; get radius
     (println
     (format "%-8s %12.2f %18.0f"
      planet
      orb-rad
      (mul au orb-rad))))

;; When you want to manipulate ﬂoating-point numbers, use the ﬂoating-point arithmetic
;; operators add, sub, mul, div rather than +, -, *, and /, which work with (and convert
;; values to) integers.

;; 4.10.1 Replacing sublists in association lists

;; To change values stored in an association list, use the assoc function as before, to ﬁnd the
;; matching sublist, then use setf on that sublist to change the value to a new sublist.

(setf (assoc "Jupiter" sol-sys) '("Jupiter" 11.2 318 5.20 11.86 1.31 0.0484
0.414 64))


;; 4.10.2 Deleting sublists from association lists

;; To delete a sublist from an association list, use the remove function. This function
;; takes a value to remove and a function that returns a boolean value indicating whether
;; the value should be removed. In this case, we'll use the keyword of the sublist
(remove 'Earth sol-sys)
;-> (("Venus" 0.949 0.82 0


;; 4.11 Summary

;; This chapter has provided a basic introduction to newLISP and has shown how to create
;; simple functions, manipulate lists, and use association lists. I've also shown
;; some simple examples of how to use these techniques to solve common programming problems.
;; If you want to learn more about newLISP or any other programming language, I recommend
;; checking out the official newLISP documentation, available at
;; http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
;; or by searching online for tutorials and books.
;; I hope this chapter has been helpful. If you have any more questions, please don't hesit
;-> 14

(remove 'Uranus sol-sys)


;; 4.11 Summary

;; This chapter has provided a basic introduction to newLISP and has shown how to create
;; simple functions, manipulate lists, and use association lists. I've also shown
;; some simple examples of how to use these techniques to solve common programming problems.
;; If you want to learn more about newLISP or any other programming language, I recommend
;; checking out the official newLISP documentation, available at
;; http://www.lispworks.com/documentation/HyperSpec/Front/index.htm
;; or by searching online for tutorials and books.
;; I hope this chapter has been helpful. If you have any more questions, please don't hesit
;-> 14

(assoc 'Earth sol-sys)
;-> ("Earth" 1.00 1.00 1.00
;     1.00 0.00 0.0167 1
;     0.00 0.00 0.00 0.0

;; 4.12 Conclusion

;; This chapter has provided a basic introduction to newLISP and has shown how to create
;; simple functions, manipulate lists, and use association lists. I've also shown
;; some simple examples of how to use these techniques to solve common programming problems.

;; 4.10.1 Replacing sublists in association lists
;; To change values stored in an association list, use the assoc function as before, to ﬁnd the
;; matching sublist, then use setf on that sublist to change the value to a new sublist.

(setf (assoc "Jupiter" sol-sys) '("Jupiter" 11.2 318 5.20 11.86 1.31 0.0484
0.414 64))

;; 4.10.2 Adding new items to association lists

;; Association lists are ordinary lists, too, so you can use all the familiar newLISP techniques
;; with them. Want to add a new 10th planet to our sol-sys list? Just use push:

(push '("Sedna" 0.093 0.00014 .0001 502 11500 0 20 0) sol-sys -1)

;; You can use sort to sort the association list. (Remember though that sort changes lists
;; permanently.) Here's a list of planets sorted by mass. Since you don't want to sort them
;; by name, you use a custom sort (see sort and randomize31 ) to compare the mass (index 2)
;; values of each pair:

(constant 'mass 2)
(sort sol-sys (fn (x y) (> (x mass) (y mass))))
(println sol-sys)

;; ("Jupiter" 11.2 318 5.2 11.86 1.31 0.0484 0.414 63)
;; ("Saturn" 9.41 95 9.54 29.46 2.48 0.0542 0.426 49)
;; ("Neptune" 3.81 17.2 30.06 164.8 1.77 0.0086 0.671 13)
;; ("Uranus" 3.98 14.6 19.22 84.01 0.77 0.0472 -0.718 27)
;; ("Earth" 1 1 1 1 0 0.0167 1 1)
;; ("Venus" 0.949 0.82 0.72 0.615 3.39 0.0068 -243 0)
;; ("Mars" 0.53 0.11 1.52 1.88 1.85 0.0934 1.03 2)
;; ("Mercury" 0.382 0.06 0.387 0.241 7 0.206 58.6 0)
;; ("Pluto" 0.18 0.002 39.5 248.5 17.1 0.249 -6.5 3)

;; You can also easily combine the data in the association list with other lists:
; restore to standard order - sort by orbit radius
(sort sol-sys (fn (x y) (< (x 3) (y 3))))
; define Unicode symbols for planets
(set 'unicode-symbols
'(("Mercury" 0x263F )
("Venus" 0x2640 )
("Earth" 0x2641 )
("Mars" 0x2642 )
("Jupiter" 0x2643 )
("Saturn" 0x2644 )
("Uranus" 0x2645 )
("Neptune" 0x2646 )
("Pluto" 0x2647)))
(map
(fn (planet)
(println (char (lookup (first planet) unicode-symbols))
"\t"
(first planet)))
sol-sys)

;; Here we've created a temporary inline function that map applies to each planet in sol-sys
;; - lookup ﬁnds the planet name and retrieves the Unicode symbol for that planet from the
;; unicode-symbols association list.
;; You can quickly remove an element from an association list with pop-assoc.

(pop-assoc (sol-sys "Pluto"))


;; This removes the Pluto element from the list.
;; newLISP oﬀers powerful data storage facilities in the form of contexts, which you can use for
;; building dictionaries, hash tables, objects, and so on. You can use association lists to build
;; dictionaries, and work with the contents of dictionaries using association list functions. See
;; Introducing contexts32 .
;; You can also use a database engine - see Using a SQLite database33 .

;; 4.10.3 ﬁnd-all and association lists

;; Another form of ﬁnd-all lets you search an association list for a sublist that matches a
;; pattern. You can specify the pattern with wildcard characters. For example, here's an
;; association list:

(set 'symphonies
'((Beethoven 9)
(Haydn 104)
(Mozart 41)
(Mahler 10)
(Wagner 1)
(Schumann 4)
(Shostakovich 15)
(Bruckner 9)))

;; You can ﬁnd all the symphonies with 9 notes:
;; To ﬁnd all the sublists that end with 9, use the match pattern (? 9), where the question
;; mark matches any single item:

(find-all '(? 9) symphonies)
;-> ((Beethoven 9) (Bruckner 9))

;; (For more about match patterns - wild card searches for lists - see matching patterns in
;; lists34 .)
;; You can also use this form with an additional action expression after the association list:
(find-all '(? 9) symphonies
    (println (first $0) { wrote 9 symphonies.}))


;; Here, the action expression uses $0 to refer to each matched element in turn.

;; 5.1 Strings in newLISP code

;; You can write strings in three ways:
;; • enclosed in double quotes
;; • embraced by curly braces
;; • marked-up by markup codes
;; like this:

(set 's "this is a string")
(set 's {this is a string})
(set 's [text]this is a string[/text])

;; All three methods can handle strings of up to 2048 characters. For strings longer than 2048
;; characters, always use the [text] and [/text] tags to enclose the string.
;; Always use the ﬁrst method, quotation marks, if you want escaped characters such as \n
;; and \t, or code numbers (\046), to be processed.

(set 's "this is a string \n with two lines")
     (println s)
;; this is a string
;; with two lines

(println "\110\101\119\076\073\083\080")

; decimal ASCII

(println "\x6e\x65\x77\x4c\x49\x53\x50")
; hex ASCII

;; Use the second method, braces (or 'curly brackets'), for strings shorter than 2048 characters
;; when you don't want any escaped characters to be processed:


(set 's {this is a string})
(println s)
;; this is a string
;; 5.2 Strings and characters

;; This is a really useful way of writing strings, because you don't have to worry about putting
;; backslashes before every quotation character, or backslashes before other backslashes. You
;; can nest pairs of braces inside a braced string, but you can't have an unmatched brace. I like
;; to use braces for strings, because they face the correct way (which plain dumb quotation
;; marks don't) and because your text editor might be able to balance and match them.
;; The third method, using [text] and [/text] markup tags, is intended for longer text strings
;; running over many lines, and is used automatically by newLISP when it outputs large
;; amounts of text. Again, you don't have to worry about which characters you can and can't
;; include - you can put anything you like in, with the obvious exception of [/text]. Escape
;; characters such as \n or \046 aren't processed either.

(set 'novel (read-file {my-latest-novel.txt}))
;->
;; [text]
;; It was a dark and "stormy" night...
;; ...
;; The End.
;; [/text]
;; If you want to know the length of a string, use length:
(length novel)
;-> 575196

;; You can access individual characters in a string using the index syntax:
(print (nth 10 novel))
;-> " "
;; If you want to get a substring, use the subseq function:
(print (subseq novel 1000 2000))
;; You can also use the string-ref function to get a character at a particular index:
(print (string-ref novel 1000))
;-> " "
;; If you want to replace a substring, use the string-replace function:
(set 'novel (string-replace novel "stormy" "calm"))
;; If you want to convert a string to a number, use the read function with the :base
;; option:
(print (read "123" :base 10))
;-> 123
;; If you want to convert a number to a string, use the number->string function:
(print (number->string 123))
;-> "123"
;; If you want to split a string into a list of substrings, use the split-string
;; function:
(print (split-string novel " "))
;; You can also use the string-split function to split a string into a list of substrings
;; with a particular separator:
(print (string-split novel "..." ""))
;; 5.3 Strings and characters: more examples

;; Here are some more examples of strings and characters:
(set 's1 "Hello")
(set 's2 "World")
(print (concatenate s1 s2))
;-> "HelloWorld"
(print (concatenate s1 " " s2))
;-> "Hello World"
(print (concatenate "The length of s1 is " (number->string (length s1))))


;; Here are some more examples of strings and characters:
(set 's1 "Hello")
(set 's2 "World")
(print (concatenate s1 s2))
;-> "HelloWorld"
(print (concatenate s1 " " s2))
;-> "Hello World"
(print (concatenate "The length of s1 is " (number->string (length s1))))
;; Here are some more examples of strings and characters:
(set 's1 "Hello")
(set 's2 "World")
(print (concatenate s1 s2))
;-> "HelloWorld"
(print (concatenate s1 " " s2))
;-> "Hello World"

;; Strings of millions of characters can be handled easily by newLISP.
;; Rather than length, use utf8len to get the length of a Unicode string:
(length (char 955))
;-> 1
(length (char 955))
;-> 2

;; 5.2 Making strings

;; Many functions, such as the ﬁle-reading ones, return strings or lists of strings for you. But
;; if you want to build a string from scratch, one way is to start with the char function. This
;; converts the supplied number to the equivalent character string with that code number.
;; It can also reverse the operation, converting the supplied character string to its equivalent
;; code number.)

(char 33)
;-> "!"

(char "!")
;-> 33
(char 955)
; Unicode lambda character, decimal code
;-> "\206\187"
(char 0x2643)
; Unicode symbol for Jupiter, hex code
;-> "\226\153\131"


;; These last two examples are available when you're running the Unicode-capable version
;; of newLISP. Since Unicode is hexadecimally inclined, you can give a hex number, starting
;; with 0x, to char. To see the actual characters, use a printing command:

(println (char 955))
;; λ
;-> "\206\187"
(println (char 0x2643))


"\226\140\152"
(println (char (int (string "0x" "2643"))))
; equivalent

;; The backslashed numbers are the result of the println function, presumably the multi-byte
;; values of the Unicode glyph.
;; You can use char to build strings in other ways:


(join (map char (sequence (char "a") (char "z"))))
;-> "abcdefghijklmnopqrstuvwxyz"

;; This uses char to ﬁnd out the ASCII code numbers for a and z, and then uses sequence to
;; generate a list of code numbers between the two. Then the char function is mapped onto
;; every element of the list, so producing a list of strings. Finally, this list is converted to a
;; single string by join.
;; join can also take a separator when building strings:

(join (map char (sequence (char "a") (char "z"))) "-")
;-> "a-b-c-d-e-f-g-h-i-j-k-l-m-n-o-p-q-r-s-t-u-v-w-x-y-z"


;; but even more useful is string, which turns any collection of numbers, lists, and strings
;; into a single string.

(string '(sequence 1 10) { produces } (sequence 1 10) "\n")
;-> (sequence 1 10) produces (1 2 3 4 5 6 7 8 9 10)

;; Notice that the ﬁrst list wasn't evaluated (because it was quoted) but that the second list
;; was evaluated to produce a list of numbers, and the resulting list - including the parentheses
;; - was converted to a string.
;; The string function, combined with the various string markers such as braces and markup
;; tags, is one way to include the values of variables inside strings:


;; You can also use format to combine strings and symbol values. See Formatting strings1 .
;; dup makes copies:

(dup "spam" 10)
;-> "spamspamspamspamspamspamspamspamspamspam"
;; And date makes a date string:
(date)
;-> "Wed Jan 25 15:04:49 2006"
;; or you can give it a number of seconds since 1970 to convert:
(date 1230000000)
;-> "Tue Dec 23 02:40:00 2008"

;; Now t has changed for ever. However, the case-changing functions aren't destructive, pro-
;; ducing new strings without harming the old ones:

(set 't "a hypothetical one-dimensional subatomic particle")
(upper-case t)
;-> "A HYPOTHETICAL ONE-DIMENSIONAL SUBATOMIC PARTICLE"

(lower-case t)
;-> "a hypothetical one-dimensional subatomic particle"
(title-case t)
;-> "A hypothetical one-dimensional subatomic particle"

;; 5.4 Substrings
;; If you know which part of a string you want to extract, use one of the following constructive
;; functions:

(set 't "a hypothetical one-dimensional subatomic particle")
(first t)
;-> "a"
(rest t)
;-> " hypothetical one-dimensional subatomic particle"
(last t)
;-> "e"
(t 2)

;-> "h"
; counting from 0
;; You can also use this technique with lists. See Selecting items from lists4 .


;; There's a shortcut to do this, too. Put the required start and length before 
;; the string in a
;; list:

(15 13 t)
;-> "one-dimension"
(0 14 t)
;-> "a hypothetical"

