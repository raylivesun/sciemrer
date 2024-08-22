;; 5.4.2 Changing the ends of strings

;; trim and chop are both constructive string-editing functions that work from the ends of
;; the original strings inwards.
;; chop works from the end:

(chop t)
; defaults to the last character
;-> "a hypothetical one-dimensional subatomic particl"
(chop t 9)
; chop 9 characters off
;-> "a hypothetical one-dimensional subatomic"


;; trim works from the ends:

(trim t)
; defaults to whitespace characters (space, newline, tab, etc.)

;; trim can remove characters from both ends:
(set 's "
    (trim s)
;-> "centred"
centred
")
; defaults to removing spaces
(set 's "------centred------")
(trim s "-")
;-> "centred"
(set 's "------centred******")
(trim s "-" "*")
; front and back
;-> "centred"


;; 5.4.3 Changing the case of characters

;; to-upper and to-lower are both case-changing functions.
(to-upper "hello, world!")
;-> "HELLO, WORLD!"
(to-lower "HELLO, WORLD!")
;-> "hello, world!"

;; pop always returns what was popped, but push returns the modiﬁed target of the action.
;; It's useful when you want to break up a string and process the pieces as you go. For
;; example, to print the newLISP version number, which is stored as a 4 or 5 digit integer,
;; use something like this:

(set 'version-string (string (sys-info -2)))
; eg: version-string is now "10303"
(set 'dev-version (pop version-string -2 2))
; always two digits
; dev-version is "03", version-string is "103"
(set 'point-version (pop version-string -1))
; always one digit
; point-version is "3", version-string is now "10"
(set 'version version-string)
; one or two digits
(println version "." point-version "." dev-version)


;; It's easier to work from the right-hand side of the string and use pop to extract the infor-
;; mation and remove it in one operation.

;; 5.5 Modifying strings

;; There are two approaches to changing characters inside a string. Either use the index
;; numbers of the characters, or specify the substring you want to ﬁnd or change.


;; 5.5.1 Changing characters by index

;; 5.5.1 Using index numbers in strings

;; To change characters by their index numbers, use setf, the general purpose function for
;; changing strings, lists, and arrays:

(set 't "a hypothetical one-dimensional subatomic particle")
(setf (t 0) "A")


;; Here's how to 'increment' the ﬁrst (zeroth) letter of a string:
(set 'wd "cream")
;-> "cream"
(setf (wd 0) (char (+ (char $it) 1)))
;-> "d"
wd
;-> "dream"

;; $it contains the value found by the ﬁrst part of the setf expression, and its numeric value
;; is incremented to form the second part.

;; 5.5.2 Changing substrings

;; If you don't want to - or can't - use index numbers or character positions, use replace, a
;; powerful destructive function that does all kinds of useful operations on strings. Use it in
;; the form:

(replace old-string source-string replacement)


;; For example, to change all occurrences of "one" to "many":
(set 't "a hypothetical one-dimensional subatomic particle")
(replace "one" "many" t)
;;-> "a hypothetical many-dimensional subatomic particle"

(set 't "a hypothetical one-dimensional subatomic particle")
(replace "hypoth" t "theor")
;-> "a theoretical one-dimensional subatomic particle"


;; Replace also supports regular expressions, which can be handy for more complex changes.
;; For example, to change all occurrences of "one-dimensional" to "three-dimensional":
(set 't "a hypothetical one-dimensional subatomic particle")
(replace #P"one-dimensional" "three-dimensional" t)
;;-> "a hypothetical three-dimensional subatomic particle"

;; 5.6 String manipulation functions

;; 5.6.1 String length


;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; 5.6.2 String concatenation

;; To concatenate strings, use the string concatenation function, which is the same as the
;; + operator:

(concatenate 'list "hello" " " "world!")
;-> ("hello" " " "world!")

;; 5.6.3 String comparison

;; Strings are compared lexicographically, meaning that they are compared character by
;; character. If they are found to be equal up to the point where they differ, the longer
;; string is considered to be greater.

;; For example:
(string= "hello" "hello")
;-> t
(string= "hello" "world")
;-> nil

;; 5.6.4 String search

;; To search for a substring within a string, use find:

(find "one-dimensional" "subatomic particle")
;-> 12


;; 5.5.3 Regular expressions

;; replace is one of a group of newLISP functions that accept regular expressions for deﬁning
;; patterns in text. For most of them, you add an extra number at the end of the expression
;; which speciﬁes options for the regular expression operation: 0 means basic matching, 1
;; means case-insensitive matching, and so on.

(set 't "a hypothetical one-dimensional subatomic particle")
(replace {h.*?l(?# h followed by l but not too greedy)} t {} 0)

;-> "a one-dimensional subatomic particle"


;; If you're happy working with Perl-compatible Regular Expressions (PCRE), you'll be happy
;; with replace and its regex-using cousins (ﬁnd, regex, ﬁnd-all, parse, starts-with, ends-
;; with, directory, and search ). Full details are in the newLISP reference manual.
;; You have to steer your pattern through both the newLISP reader and the regular expression
;; processor. Remember the diﬀerence between strings enclosed in quotes and strings enclosed
;; in braces? Quotes allow the processing of escaped characters, whereas braces don't. Braces
;; have some advantages: they face each other visually, they don't have smart and dumb
;; versions to confuse you, your text editor might balance them for you, and they let you
;; use the more commonly occurring quotation characters in strings without having to escape
;; them all the time. But if you use quotes, you must double the backslashes, so that a single
;; backslash survives intact as far as the regular expression processor:


;; 5.6.5 String manipulation functions

;; 5.6.5.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; 5.6.5.2 String concatenation
;; To concatenate strings, use the string concatenation function, which is the same as the
;; + operator:

(concatenate 'list "hello" " " "world!")
;-> ("hello" " " "world!")

;; 5.6.5.3 String comparison
;; Strings are compared lexicographically, meaning that they are compared character by
;; character. If they are found to be equal up to the point where they differ, the longer

(set 'str "\s")
(replace str "this is a phrase" "|" 0)
space) ...
;-> thi| i| a phra|e
(set 'str "\\s")
(replace str "this is a phrase" "|" 0)
;-> this|is|a|phrase
; oops, not searching for \s (white
; but for the letter s
; ah, better!


;; 5.6.5.4 String search

;; To search for a substring within a string, use find:

(find "one-dimensional" "subatomic particle")
;-> 12

;; 5.6.5.5 String manipulation functions

;; 5.6.5.5.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; 5.6.5.5.2 String concatenation
;; To concatenate strings, use the string concatenation function, which is the same as the
;; + operator:

(concatenate 'list "hello" " " "world!")
;-> ("hello" " " "world!")

;; 5.6.5.5.3 String comparison
;; Strings are compared lexicographically, meaning that they are compared character by
;; character. If they are found to be equal up to the point where they differ, the longer
;; string is considered to be greater.

;; 5.6.5.5.4 String search

;; To search for a substring within a string, use find:

(find "one-dimensional" "subatomic particle")
;-> 12

;; 5.6.5.5.5 String manipulation functions

;; 5.6.5.5.5.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13


;; 5.5.4 System variables: $0, $1 ...

;; replace updates a set of system variables $0, $1, $2, up to $15, with the matches. These
;; refer to the parenthesized expressions in the pattern, and are the equivalent of the \1, \2
;; that you might be familiar with if you've used grep. For example:

(set 'quotation {"I cannot explain." She spoke in a low, eager voice,
with a curious lisp in her utterance. "But for God's sake do what I
ask you. Go back and never set foot upon the moor again."})
(replace {(.*?),.*?curious\s*(l.*p\W)(.*?)(moor)(.*)}
quotation
(println {$1 } $1 { $2 } $2 { $3 } $3 { $4 } $4 { $5 } $5)
4)

;; $1 "I cannot explain." She spoke in a low $2 lisp $3 in her utterance.
;; "But for God's sake do what I ask you. Go back and never set foot upon
;; the $4 moor $5 again."

;; Here we've looked for ﬁve patterns, separated by any string starting with a comma and
;; ending with the word curious. $0 stores the matched expression, $1 stores the ﬁrst paren-
;; thesized sub-expression, and so on.

;; If you prefer to use quotation marks rather than the braces I used here, remember that
;; certain characters have to be escaped with a backslash.


;; 5.6.5.5.5.2 String manipulation functions

;; 5.6.5.5.5.2.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; 5.5.5 The replacement expression

;; The previous example demonstrates that an important feature of replace is that the re-
;; placement doesn't have to be just a simple string or list, it can be any newLISP expression.
;; Each time the pattern is found, the replacement expression is evaluated. You can use this
;; to provide a replacement value that's calculated dynamically, or you could do anything else
;; you wanted to with the found text. It's even possible to evaluate an expression that's got
;; nothing to do with found text at all.
;; Here's another example: search for the letter t followed either by the letter h or by any
;; vowel, and print out the combinations that replace found:

(set 't "a hypothetical one-dimensional subatomic particle")
(replace {t[h]|t[aeiou]} t (println $0) 0)


;; 5.6.5.5.5.2.2 String concatenation

;; To concatenate strings, use the string concatenation function, which is the same as the
;; + operator:

(concatenate 'list "hello" " " "world!")
;-> ("hello" " " "world!")

;; 5.6.5.5.5.2.3 String comparison

;; Strings are compared lexicographically, meaning that they are compared character by
;; character. If they are found to be equal up to the point where they differ, the longer

(set 'str "\s")
(replace str "this is a phrase" "|" 0)
space) ...
;-> thi| i| a phra|e
(set 'str "\\s")
(replace str "this is a phrase" "|" 0)
;-> this|is|a|phrase
; oops, not searching for \s (white
; but for the letter s
; ah, better!

;; 5.6.5.5.5.2.4 String search

;; To search for a substring within a string, use find:

(find "one-dimensional" "subatomic particle")
;-> 12

;; 5.6.5.5.5.2.5 String manipulation functions
;-> "a hypothetical one-dimensional subatomic particle"
;; For every matching piece of text found, the third expression

(println $0)
;-> a
;-> hypothetical
;-> one-dimensional
;-> subatomic
;-> particle

;; 5.6.5.5.5.2.6 String manipulation functions

;; 5.6.5.5.5.2.6.1 String length


;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; was evaluated. This is a good way of seeing what the regular expression engine is up to
;; while the function is running. In this example, the original string appears to be unchanged,
;; but in fact it did change, because (println $0) did two things: it printed the string, and it
;; returned the value to replace, thus replacing the found text with itself. Invisible mending!
;; If the replacement expression doesn't return a string, no replacement occurs.
;; You could do other useful things too, such as build a list of matches for later processing,
;; and you can use the newLISP system variables and any other function to use any of the
;; text that was found.
;; In the next example, we look for the letters a, e, or c, and force each occurrence to upper-
;; case:

(replace "a|e|c" "This is a sentence" (upper-case $0) 0)
;-> "This is A sEntEnCE"


;; 5.6.5.5.5.2.7 String manipulation functions
;; 5.6.5.5.5.2.7.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; 5.5.6 The pattern

;; The pattern for the previous example was a string containing a single character, but
;; in the next example, we use a regular expression that matches any sequence of
;; characters. The pattern is ".*", which means "any sequence of characters". Here's how
;; the function works:

;; 1. The pattern is scanned from left to right.
;; 2. When a match is found, the pattern is replaced with the replacement expression,
;; and the function returns the result.
;; 3. If no match is found, the function returns the original string.
;; Here's the next example: search for any sequence of characters, and print out
(set 'any_sequence ".*")
(replace any_sequence "this is a sentence" (println $0) 0)

;; 5.6.5.5.5.2.8 String manipulation functions

;; As another example, here's a simple search and replace operation that keeps count of how
;; many times the letter 'o' has been found in a string, and replaces each occurrence in the
;; original string with the count so far. The replacement is a block of expressions grouped
;; into a single begin expression. This block is evaluated every time a match is found:

(set 't "a hypothetical one-dimensional subatomic particle")
(set 'counter 0)
(replace "o" t
(begin
(inc counter)
(println {replacing "} $0 {" number } counter)
(string counter))
; the replacement text should be a string
0)

;; The output from println doesn't appear in the string; the ﬁnal value of the entire begin
;; expression is a string version of the counter, so that gets inserted into the string.
;; Here's yet another example of replace in action. Suppose I have a text ﬁle, consisting of
;; the following:


;; 5.6.5.5.5.2.9 String manipulation functions
;; 5.6.5.5.5.2.9.1 String length

;; To get the length of a string, use length:

(length "hello, world!")
;-> 13

;; The output from println doesn't appear in the string; the ﬁnal value of the entire begin
;; expression is a string version of the counter, so that gets inserted into the string.
;; Here's yet another example of replace in action. Suppose I have a text ﬁle, consisting of
;; the following:

a = 15
another_variable = "strings"
x2 = "another string"
c = 25
3x=9

;; I want to write a newLISP script that re-numbers the lines in multiples of 10, starting at
;; 10, and aligns the text so that the equals signs line up, like this:

10  a = 15
20  another_variable = "strings"
30  x2 = "another string"
40  c = 25

;;(I don't know what language this is!)
;; The following script will do this:
(set 'file (open ((main-args) 2) "read"))
     (set 'counter 0)
     (while (read-line file)
     (set 'temp
(replace {^(\d*)(\s*)(.*)}
; the numbering
     (current-line)
     (string (inc counter 10) " " $3)
0))
(println
   (replace {(\S*)(\s*)(=)(\s*)(.*)} ; the spaces around =
    temp
   (string $1 (dup " " (- 20 (length $1))) $3 " " $5)
0)))
(exit)

;; I've used two replace operations inside the while loop, to keep things clearer. The ﬁrst
;; one sets a temporary variable to the result of a replace operation. The search string
;; ({ˆ(\d*)(\s*)(.*)}) is a regular expression that's looking for any number at the start
;; of a line, followed by some space, followed by anything. The replacement string ((string
;; (inc counter 10) " " $3) 0)) consists of a incremented counter value, followed by the
;; third match (ie the anything I just looked for).
;; The result of the second replace operation is printed. I'm searching the temporary variable
;; temp for more strings and spaces with an equals sign in the middle:

({(\S*)(\s*)(=)(\s*)(.*)})


;; The replacement expression is built up from the important found elements ($1, $3, $5) but
;; it also includes a quick calculation of the amount of space required to bring the equals sign
;; across to character 20, which should be the diﬀerence between the ﬁrst item's width and
;; position 20 (which I've chosen arbitrarily as the location for the equals sign).
;; Regular expressions aren't very easy for the newcomer, but they're very powerful, particu-
;; larly with newLISP's replace function, so they're worth learning.


;; 5.6 Testing and comparing strings
;; There are various tests that you can run on strings. newLISP's comparison operators work
;; by ﬁnding and comparing the code numbers of the characters until a decision can be made:

(> {Higgs Boson} {Higgs boson})   ; nil
(> {Higgs Boson} {Higgs})         ; true
(< {dollar} {euro})               ; true
(> {newLISP} {LISP})              ; true


(= {fred} {Fred})                 ; nil   
(= {fred} {fred})                 ; true



;; These comparison functions also let you use them with a single argument. If you supply
;; only one argument, newLISP helpfully assumes that you mean 0 or "", depending on the
;; type of the ﬁrst argument:

(> 1)             ;; true - assume > 0
(> "fred")        ;; true - assume > ""   

;; To check whether two strings share common features, you can either use starts-with and
;; ends-with, or the more general pattern matching commands member, regex, ﬁnd, and
;; ﬁnd-all. starts-with and ends-with are simple enough:

(starts-with "newLISP" "new")   ; does newLISP start with new?
;-> true
(ends-with "newLISP" "LISP")
;-> true


;; They can also accept regular expressions, using one of the regex options (0 being the most
;; commonly used):

(starts-with {newLISP} {[a-z][aeiou](?\#lc followed by lc vowel)} 0)
;-> true
(ends-with {newLISP} {[aeiou][A-Z](?\# lc vowel followed by UCase)} 0)
;-> false

;; ﬁnd, ﬁnd-all, member, and regex look everywhere in a string. ﬁnd returns the index of
;; the matching substring:

(set 't "a hypothetical one-dimensional subatomic particle")
(find "atom" t)
;-> 34
(find "l" t)
;-> 13
(find "L" t)            ; search is case-sensitive
;-> nil

;; member looks to see if one string is in another. It returns the rest of the string, including
;; the search string, rather than the index of the ﬁrst occurrence.

(member "rest" "a good restaurant")
;-> "restaurant"

;; Both ﬁnd and member let you use regular expressions:

(set 'quotation {"I cannot explain." She spoke in a low,
eager voice, with a curious lisp in her utterance. "But for
Gods sake do what I ask you. Go back and never set foot upon
the moor again."})


(find {(\S*)} quotation)
;-> "I cannot explain."
(member {(\S*)} quotation)
;-> "I cannot explain. She spoke in a low, eager voice, with a curious

;; 5.6.5.5.5.2.10 String manipulation functions


;; The following functions are used for string manipulation:

(find "lisp" quotation)
;-> 69; without regex
; character 69
(find {i} quotation 0)
;-> 15; with regex
; character 15
(find {s} quotation 1)
;-> 20; case insensitive regex
; character 20
(println "character "
(find {(l.*?p)} quotation 0) ": " $0)
;-> character 13: lain." She sp
; l followed by a p

;; ﬁnd-all works like ﬁnd, but returns a list of all matching strings, rather than the index of
;; just the ﬁrst match. It always takes regular expressions, so - for once - you don't have to
;; put regex option numbers at the end.

(set 'quotation {"I cannot explain." She spoke in a low,
eager voice, with a curious lisp in her utterance. "But for
Gods sake do what I ask you. Go back and never set foot upon
the moor again."})

(find-all "[aeiou]{2,}" quotation $0)
; two or more vowels
;-> ("ai" "ea" "oi" "iou" "ou" "oo" "oo" "ai")

;; This results list can be interpreted as 'the ﬁrst match was from character 0 continuing for 70
;; characters, the second from character 0 continuing for 15 characters, another from character
;; 15 for 33 characters', and so on.
;; The matches are also stored in the system variables ($0, $1, ...) which you can inspect
;; easily with a simple loop:

(for (x 1 4)
(println {$} x ": " ($ x)))


;; 5.7 Strings to lists

;; Two functions let you convert strings to lists, ready for manipulation with newLISP's ex-
;; tensive list-processing powers. The well-named explode function cracks open a string and
;; returns a list of single characters:

(set 't "a hypothetical one-dimensional subatomic particle")
(explode t)

;; :-> ("a" " " "h" "y" "p" "o" "t" "h" "e" "t" "i" "c" "a" "l"
;; " " "o" "n" "e" "-" "d" "i" "m" "e" "n" "s" "i" "o" "n" "a"
;; "l" " " "s" "u" "b" "a" "t" "o" "m" "i" "c" " " "p" "a" "r"
;; "t" "i" "c" "l" "e")

;; The explosion is easily reversed with join. explode can also take an integer. This deﬁnes
;; the size of the fragments. For example, to divide up a string into cryptographer-style 5
;; letter groups, remove the spaces and use explode like this:

(explode (replace " " t "") 5)
;-> ("ahypo" "theti" "calon" "e-dim" "ensio" "nalsu" "batom" "icpar" "ticle")

;; You can do similar tricks with ﬁnd-all. Watch the end, though:

(find-all ".{3}" t)

; this regex drops chars!
;-> ("a h" "ypo" "the" "tic" "al " "one" "-di" "men"
; "sio" "nal" " su" "bat" "omi" "c p" "art" "icl")

;; 5.8 Parsing strings

;; parse is a powerful way of breaking strings up and returning the pieces. Used on its own,
;; it breaks strings apart, usually at word boundaries, eats the boundaries, and returns a list
;; of the remaining pieces:

(parse t)

; defaults to spaces...
;-> ("a" "hypothetical" "one-dimensional" "subatomic" "particle")

;; Or you can supply a delimiting character, and parse breaks the string whenever it meets
;; that character:

(set 'pathname {/System/Library/Fonts/Courier.dfont})
(parse pathname {/})
;-> ("" "System" "Library" "Fonts" "Courier.dfont")

;; By the way, I could eliminate that ﬁrst empty string from the list by ﬁltering it out:

(clean empty? (parse pathname {/}))
;-> ("System" "Library" "Fonts" "Courier.dfont")

;; You can also specify a delimiter string rather than a delimiter character:

(set 't (dup "spam" 8))
;-> "spamspamspamspamspamspamspamspam"

(parse t {am})

; break on "am"
;-> ("sp" "sp" "sp" "sp" "sp" "sp" "sp" "sp" "")

;; Best of all, though, you can specify a regular expression delimiter. Make sure you supply
;; the options ﬂag (0 or whatever), as with most of the regex functions in newLISP:

;; Here's that well-known quick and not very reliable HTML-tag stripper:

(set 'html (read-file "/Users/Sites/index.html"))
(println (parse html {<.*?>} 4))

; option 4: dot matches newline

;; For parsing XML strings, newLISP provides the function xml-parse. See Working with
;; XML5 .
;; Take care when using parse on text. Unless you specify exactly what you want, it thinks
;; you're passing it newLISP source code. This can produce surprising results:

(set 't {Eats, shoots, and leaves ; a book by Lynn Truss})
(parse t)

;-> ("Eats" "," "shoots" "," "and" "leaves")
; she's gone!

;; The semicolon is considered a comment character in newLISP, so parse has ignored it and
;; everything that followed on that line. Tell it what you really want, using delimiters or
;; regular expressions:

(set 't {Eats, shoots, and leaves ; a book by Lynn Truss})
(parse t " ")

;-> ("Eats," "shoots," "and" "leaves" ";" "a" "book" "by" "Lynn" "Truss")


;; 5.9 Strings to numbers


;; newLISP has several functions to convert strings to numbers:

(set 't "42")

(number? t)
;-> true

(parse t)
;-> (42)

(parse t 0)
;-> 42

(parse t 0.0)
;-> 42.0

(parse t 0.0 0)
;-> 42

(parse t 0.0 0.0)
;-> 42.0

;; If the string can't be converted to a number, the function will return nil:

(parse "not a number")
;-> nil


;; 5.10 Numbers to strings

;; newLISP has several functions to convert numbers to strings:

(set 't 42.0)

(number->string t)
;-> "42.0"

(number->string t 2)
;-> "42.00"

(number->string t 2 0)
;-> "42"

(number->string t 2 0.0)
;-> "42.0"

;; If the number is a whole number, the decimal point and trailing zeros are omitted:

(number->string 42)
;-> "42"

;; If the number is negative, the minus sign is included:

(number->string -42)
;-> "-42"

(parse t "\\s" 0)
; white space
;-> ("Eats," "shoots," "and" "leaves" ";" "a" "book" "by" "Lynn" "Truss")

;; 5.11 Strings to characters


;; newLISP has a function to convert a string to a single character:

(set 't "a")

(string->char t)
;-> #\a

;; If the string contains more than one character, the function will return the character at
;; the 0th index:

(set 't "hello")
(string->char t)
;-> #\h

;; If the string is empty, the function will return nil:

(set 't "")
(string->char t)
;-> nil

;; 5.12 Characters to strings

;; newLISP has a function to convert a character to a string:

(char->string #\a)
;-> "a"

;; If the character is not a printable character, the function will return a string
;; representation of its character code:

(char->string #\newline)
;-> "\n"

;; 5.13 Strings to lists of characters


;; newLISP has a function to convert a string to a list of characters:

(set 't "hello")
(string->list t)
;-> (#\h #\e #\l #\l #\o)

;; If the string is empty, the function will return an empty list:

(set 't "")
(string->list t)
;-> ()

;; 5.14 Lists of characters to strings

;; newLISP has a function to convert a list of characters to a string:

;; If you want to chop strings up in other ways, consider using ﬁnd-all, which returns a list
;; of strings that match a pattern. If you can specify the chopping operation as a regular
;; expression, you're in luck. For example, if you want to split a number into groups of three
;; digits, use this technique:

(set 'a "1212374192387562311")
(println (find-all {\d{3}|\d{2}$|\d$} a))
;-> ("121" "237" "419" "238" "756" "231" "1")
; alternatively
(explode a 3)
;-> ("121" "237" "419" "238" "756" "231" "1")

;; 5.15 Strings to vectors

;; newLISP has a function to convert a string to a vector:

(set 't "hello")
(string->vector t)
;-> (#\h #\e #\l #\l #\o)

;; If the string is empty, the function will return an empty vector:

(set 't "")
(string->vector t)
;-> ()

;; 5.16 Vectors to strings

;; newLISP has a function to convert a vector to a string:


;; If you want to convert a vector to a string with a delimiter, use join:

;; parse eats the delimiters once they've done their work - ﬁnd-all ﬁnds things and returns
;; what it ﬁnds.

(find-all {\w+} t )
; word characters
;-> ("Eats" "shoots" "and" "leaves" "a" "book" "by" "Lynn" "Truss")
(parse t {\w+} 0 )
; eats and leaves delimiters
;-> ("" ", " ", " " " "; " " " " " " " " " "")

;; 5.9 Other string functions
;; There are other functions that work with strings. search looks for a string inside a ﬁle on
;; disk:

(set 'f (open {/private/var/log/system.log} {read}))
(search f {kernel})
(seek f (- (seek f) 64))
; rewind file pointer
(dotimes (n 3)
(println (read-line f)))
(close f)


;; 5.10 Regular expressions

;; newLISP supports regular expressions. The function regexp-match will match a regular
;; expression against a string:


;; For example, to match a number followed by a word:

;; 5.10 Formatting strings

;; It's worth mentioning the format function, which lets you insert the values of newLISP
;; expressions into a pre-deﬁned template string. Use %s to represent the location of a string
;; expression inside the template, and other % codes to include numbers. For example, suppose
;; you want to display a list of ﬁles like this:

folder: Library
file: mach


;; Here's how you can format the string:

(format "folder: %s\nfile: %s" folder file)

;-> "folder: Library\nfile: mach"

;; A suitable template for folders (directories) looks like this:

"folder: %s" ; or
" file: %s"


;; Give the format function a template string, followed by the expression (f) that produces a
;; ﬁle or folder name:

(format "folder: %s" f) ; or
(format " file: %s" f)

;; When this is evaluated, the contents of f is inserted into the string where the %s is. The
;; code to generate a directory listing in this format, using the directory function, looks like
;; this:


;; 6.1 List manipulation

(dolist (f (directory))
        (if (directory? f)
(println (format "folder: %s" f))
(println (format " file: %s" f))))

;; I'm using the directory? function to choose the right template string. A typical listing
;; looks like this:


;; 6.2 Vector manipulation


;; There are lots of formatting codes that you use to produce the output you want. You use
;; numbers to control the alignment and precision of the strings and numbers. Just make sure
;; that the % constructions in the format string match the expressions or symbols that appear
;; after it, and that there are the same number of each.
;; Here's another example. We'll display the ﬁrst 400 or so Unicode characters in decimal,
;; hexadecimal, and binary. We'll use the bits function to generate a binary string. We feed
;; a list of three values to format after the format string, which has three entries:

(for (x 32 0x01a0)
     (println (char x)
(format "%4d\t%4x\t%10s"
(list x x (bits x)))))


;; 5.11 Strings that make newLISP think

;; Lastly, I must mention eval and eval-string. Both of these let you give newLISP code to
;; newLISP for evaluation. If it's valid newLISP, you'll see the result of the evaluation. eval
;; wants an expression:


;; eval-string is like eval, but it takes a string as input:
(set 'expr '(+ 1 2))
(eval expr)
;-> 3
;; eval-string wants a string:
(set 'expr "(+ 1 2)")
(eval-string expr)
;-> 3

;; 6.3 Summary


;; newLISP is a powerful, flexible language that allows you to create programs that do
;; amazing things. I've shown you how to create functions, manipulate lists and vectors, and
;; format strings. You can also use eval and eval-string to evaluate newLISP code.
;; If you want to learn more about newLISP, I recommend checking out the newLISP
;; website at http://www.newlisp.org/. The book "The newLISP Way


;; 7.1 Conclusion

;; This means that you can build newLISP code, using any of the functions we've met, and
;; then have it evaluated by newLISP. eval is particularly useful when you're deﬁning macros
;; - functions that delay evaluation until you choose to do it. See Macros6 .
;; You could use eval and eval-string to write programs that write programs.
;; The following curious piece of newLISP continually and mindlessly rearranges a few strings
;; and tries to evaluate the result. Unsuccessful attempts are safely caught. When it ﬁnally
;; becomes valid newLISP, it will be evaluated successfully and the result will satisfy the
;; ﬁnishing condition and ﬁnish the loop.


;; 7.2 Credits

(set 'code '(")" "set" "'valid" "true" "("))
    (set 'valid nil)
    (until valid
    (set 'code (randomize code))
    (println (join code " "))
(catch (eval-string (join code " ")) 'result))



;; 7.3 Appendices

;; A.1 Creating macros
;; Macros are functions that delay evaluation until they're called. Here's a simple
;; example of a macro that takes a list of numbers and returns their sum:

;; (defmacro sum (numbers)
;; (list '+ (mapcar #'identity numbers)))
;;
;; (sum '(1 2 3 4 5))
;; ;-> (1+ 2+ 3+ 4+ 5)
;;
;; B.1 Macros as functions

;; I've used programs that were obviously written using this programming technique...


;; 6 Apply and map: applying functions to lists

;; 6.1 Making functions and data work together

;; Often, you'll ﬁnd that you've got some data stored in a list and you want to apply a function
;; to it. For example, suppose that you've acquired some temperature readings from a space
;; probe, and they're stored in a list called data:

(println data)
(0.1 3.2 -1.2 1.2 -2.3 0.1 1.4 2.5 0.3)

