;; You can still switch to the Double context in the usual way:

(context Double)

;; newLISP is smart enough to be able to work out from your code whether to use the default
;; function of a context or the context itself.

;; 7.3.1 Passing parameters by reference

;; There are important diﬀerences between default functions when used as symbols and their
;; more ordinary siblings. When you use a default function to pass data to a function, newLISP
;; uses a reference to the data rather than a copy. For larger lists and strings, references are
;; much quicker for newLISP to pass around between functions, so your code will be faster if
;; you can use store data as a default function and use the context name as a parameter.
;; Also, and as a consequence, functions change the contents of any default functions passed
;; as reference parameters. Ordinary symbols are copied when passed as parameters. Observe
;; the following code. I'll create two symbols, one of which is a 'default function', the other is
;; a plain symbol:

(define Evens:Evens (sequence 0 30 2))
; symbol is the default function for
;; the Evens context
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)

(define odds (sequence 1 31 2))
; ordinary symbol
;-> (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31)
; this function reverses a list


(define (my-reverse lst)
(reverse lst))

(my-reverse Evens)
; default function as parameter
;-> (30 28 26 24 22 20 18 16 14 12 10 8 6 4 2 0)

(my-reverse odds)
; ordinary symbol as parameter
;-> (31 29 27 25 23 21 19 17 15 13 11 9 7 5 3 1)

;; 7.3.2 Passing parameters by value

;; So far, it looks as if they're behaving identically. But now inspect the original symbols:

Evens:Evens
(30 28 26 24 22 20 18 16 14 12 10 8 6 4 2 0)
odds
(1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31)


;; The list that was passed as a default function - as a reference - was modiﬁed, whereas the
;; ordinary list parameter was copied, as usual, and wasn't modiﬁed.


;; 7.3.3 Passing parameters by reference and by value

;; 7.3.2 Functions with a memory

;; In the following example, we create a context called Output, and a default function inside
;; it, also called Output. This function prints its arguments, and increments a counter by
;; the number of characters output. Because the default function has the same name as the
;; context, it is executed whenever we use the name of the context in other expressions.
;; Inside this function, the value of a variable counter (inside the context Output) is incre-
;; mented if it exists, or created and initialized if it doesn't. Then the function's main task
;; - the printing of the arguments - is done. The counter symbol keeps count of how many
;; characters were output.

(define (Output:Output)
; define the default function
(unless Output:counter
(set 'Output:counter 0))
(inc Output:counter (length (string (args))))
(map print (args))
(println))
(dotimes (x 90)
(Output
; use context name as a function
"the square root of " x " is " (sqrt x)))
(Output "you used " Output:counter " characters")


;; 7.3.4 Function with a memory and a local scope

;; In the following example, we create a context called Output, and a default function inside
;; it, also called Output. This function prints its arguments, and increments a counter by
;; the number of characters output. Because the default function has the same name as the
;; context, it is executed whenever we use the name of the context in other expressions.
(define (Output:Output)
; define the default function
(let ((counter 0))
(map print (args))
(println)
(inc counter (length (string (args)))))
counter)
(dotimes (x 90)
(let ((result (Output
; use context name as a function
"the square root of " x " is " (sqrt x))))
(println "You used " result " characters")))

;; 7.3.5 The default function and the context


;; 7.3.6 Using default functions as symbols

;; 7.3.7 Using default functions as ordinary symbols

;; 7.3.8 Using default functions as default functions

;; 7.3.9 Passing default functions as parameters

;; 7.3.10 Passing default functions as values

;; 7.3.11 Using default functions as contexts


;; 7.3.12 Using default functions as local scopes

;; 7.3.13 Using default functions as global scopes

;; 7.3.14 Using default functions as built-in functions

;; 7.3.15 Using default functions as modules

;; 7.3.16 Using default functions as libraries

;; 7.3.17 Using default functions as extensions

;; 7.3.18 Using default functions as plugins

;; 7.3.19 Using default functions as custom data structures

;; 7.3.20 Using default functions as custom functions

;; 7.3.21 Using default functions as custom operators

;; 7.3.22 Using default functions as custom syntax

;; 7.3.23 Using default functions as custom control structures

;; 7.3.24 Using default functions as custom data structures

;; 7.3.25 Using default functions as custom functions

;; 7.3.26 Using default functions as custom operators

;; 7.3.27 Using default functions as custom syntax

;; 7.3.28 Using default functions as custom control structures

;; 7.3.29 Using default functions as custom data structures

;; 7.3.30 Using default functions as custom functions

;; 7.3.31 Using default functions as custom operators

;; 7.3.32 Using default functions as custom syntax

;; 7.3.33 Using default functions as custom control structures

;; 7.3.34 Using default functions as custom data structures

;; 7.3.35 Using default functions as custom functions

;; 7.3.36 Using default functions as custom operators

;; 7.3.37 Using default functions as custom syntax

;; 7.3.38 Using default functions as custom control structures

;; 7.3.39 Using default functions as custom data structures

;; 7.3.40 Using default functions as custom functions

;; 7.3.41 Using default functions as custom operators

;; 7.3.42 Using default functions as custom syntax

;; 7.3.43 Using default functions as custom control structures

;; 7.3.44 Using default functions as custom data structures

;; 7.3.45 Using default functions as custom functions

;; 7.3.46 Using default functions as custom operators

;; 7.3.47 Using default functions as custom syntax

;; 7.3.48 Using default functions as custom control structures

;; 7.3.49 Using default functions as custom data structures

;; 7.3.50 Using default functions as custom functions

;; 7.3.51 Using default functions as custom operators

;; 7.3.52 Using default functions as custom syntax

;; 7.3.53 Using default functions as custom control structures

;; 7.3.54 Using default functions as custom data structures

;; 7.3.55 Using default functions as custom functions

;; 7.3.56 Using default functions as custom operators

;; 7.3.57 Using default functions as custom syntax

;; 7.3.58 Using default functions as custom control structures

;; 7.3.59 Using default functions as custom data structures

;; 7.3.60 Using default functions as custom functions

;; 7.3.61 Using default functions as custom operators

;; 7.3.62 Using default functions as custom syntax

;; 7.3.63 Using default functions as custom control structures

;; 7.3.64 Using default functions as custom data structures

;; 7.3.65 Using default functions as custom functions

;; 7.3.66 Using default functions as custom operators

;; 7.3.67 Using default functions as custom syntax

;; 7.3.68 Using default functions as custom control structures

;; 7.3.69 Using default functions as custom data structures

;; 7.3.70 Using default functions as custom functions

;; 7.3.71 Using default functions as custom operators

;; 7.3.72 Using default functions as custom syntax

;; 7.3.73 Using default functions as custom control structures

;; 7.3.74 Using default functions as custom data structures

;; 7.3.75 Using default functions as custom functions

;; 7.3.76 Using default functions as custom operators

;; 7.3.77 Using default functions as custom syntax

;; 7.3.78 Using default functions as custom control structures

;; 7.3.79 Using default functions as custom data structures

;; 7.3.80 Using default functions as custom functions

;; 7.3.81 Using default functions as custom operators

;; 7.3.82 Using default functions as custom syntax

;; 7.3.83 Using default functions as custom control structures

;; 7.3.84 Using default functions as custom data structures

;; 7.3.85 Using default functions as custom functions

;; 7.3.86 Using default functions as custom operators

;; 7.3.87 Using default functions as custom syntax

;; 7.3.88 Using default functions as custom control structures

;; 7.3.89 Using default functions as custom data structures

;; 7.3.90 Using default functions as custom functions

;; 7.3.91 Using default functions as custom operators

;; 7.3.92 Using default functions as custom syntax

;; 7.3.93 Using default functions as custom control structures

;; 7.3.94 Using default functions as custom data structures

;; 7.3.95 Using default functions as custom functions

;; 7.3.96 Using default functions as custom operators

;; 7.3.97 Using default functions as custom syntax

;; 7.3.98 Using default functions as custom control structures

;; 7.3.99 Using default functions as custom data structures

;; 7.3.100 Using default functions as custom functions

;; 7.3.101 Using default functions as custom operators

;; 7.3.102 Using default functions as custom syntax

;; 7.3.103 Using default functions as custom control structures

;; 7.3.104 Using default functions as custom data structures

;; 7.3.105 Using default functions as custom functions

;; 7.3.106 Using default functions as custom operators

;; 7.3.107 Using default functions as custom syntax

;; 7.3.108 Using default functions as custom control structures

;; 7.3.109 Using default functions as custom data structures

;; 7.3.110 Using default functions as custom functions

;; 7.3.111 Using default functions as custom operators

;; 7.3.112 Using default functions as custom syntax

;; 7.3.113 Using default functions as custom control structures


;; 7.3.114 Using default functions as custom data structures

;; 7.3.115 Using default functions as custom functions

;; 7.3.116 Using default functions as custom operators

;; 7.3.117 Using default functions as custom syntax

;; 7.3.118 Using default functions as custom control structures

;; 7.3.119 Using default functions as custom data structures

;; 7.3.120 Using default functions as custom functions

;; 7.3.121 Using default functions as custom operators

;; 7.3.122 Using default functions as custom syntax

;; 7.3.123 Using default functions as custom control structures

;; 7.3.124 Using default functions as custom data structures

;; 7.3.125 Using default functions as custom functions

;; 7.3.126 Using default functions as custom operators

;; 7.3.127 Using default functions as custom syntax

;; 7.3.128 Using default functions as custom control structures

;; 7.3.129 Using default functions as custom data structures

;; 7.3.130 Using default functions as custom functions

;; 7.3.131 Using default functions as custom operators

;; 7.3.132 Using default functions as custom syntax

;; 7.3.133 Using default functions as custom control structures

;; 7.3.134 Using default functions as custom data structures

;; The Output function eﬀectively remembers how much work it's done since it was ﬁrst cre-
;; ated. It could even append that information to a log ﬁle.
;; Think of the possibilities. You could log the usage of all your functions, and bill users
;; according to how often they use them.
;; You can override the built-in println function so that it uses this code instead when it's
;; called. See On your own terms1 .

;; 7.4 Dictionaries and tables

;; A common use for a context is a dictionary: an ordered set of unique key/value pairs,
;; arranged so that you can obtain the current value of a key, or add a new key/value pair.
;; newLISP makes creating dictionaries easy. To illustrate, I'll enlist the help of the great
;; detective, Sherlock Holmes. First, I downloaded Sir Arthur Conan Doyle's The Sign of
;; Four from Project Gutenberg, then I loaded the ﬁle as a list of words.

(set 'file "/Users/me/Sherlock Holmes/sign-of-four.txt")
(set 'data (clean empty? (parse (read-file file) "\\W" 0)))
;; remove all white-spaces, returns a list.
;read file and

;; Next, I deﬁne an empty dictionary:

(define Doyle:Doyle)

;; This deﬁnes the Doyle context and the default function, but leaves that default function
;; uninitialized. If the default function is left empty, you can use the following expressions to
;; build and examine a dictionary:
;; • (Doyle key value) - set key to value
;; • (Doyle key) - get value of key
;; • (Doyle key nil) - delete key
;; To build a dictionary from the word list, you scan through the words, and, if the word is;; 
;; not in the dictionary, add it as the key, and set the value to 1. But if the word is already
;; in the dictionary, get the value, add 1 to it, and save the new value:


;; 7.4.1 Building a dictionary from a list

;; 7.4.2 Examining a dictionary

;; 7.4.3 Using a dictionary as a context

;; 7.4.4 Using a dictionary as a function

;; 7.4.5 Using a dictionary as a data structure

;; 7.4.6 Using a dictionary as a control structure

(dolist (word data)
        (set 'lc-word (lower-case word))
        (if (set 'tally (Doyle lc-word))
(Doyle lc-word (inc tally))
(Doyle lc-word 1))) 


;; 7.4.7 Using a dictionary as a function

;; 7.4.8 Using a dictionary as a data structure

;; 7.4.9 Using a dictionary as a control structure

;; 7.4.10 Using a dictionary as a custom syntax

;; 7.4.11 Using a dictionary as a custom control structure

;; 7.4.12 Using a dictionary as a custom data structure

;; 7.4.13 Using a dictionary as a custom function

;; This shorter alternative eliminates the conditional:

(dolist (word data)
(set 'lc-word (lower-case word))
(Doyle lc-word (inc (int (Doyle lc-word)))))

(dolist (word data)
(Doyle (lower-case word) (inc (Doyle (lower-case word)))))


;; 7.4.14 Using a dictionary as a custom syntax

;; 7.4.15 Using a dictionary as a custom control structure

;; 7.4.16 Using a dictionary as a custom data structure

;; Each word is added to the dictionary, and the value (the number of occurrences) increased
;; by 1. Inside the context, the names of the keys have been preﬁxed with an underscore
;; ("_"). This is so that nobody gets confused between the names of keys and newLISP
;; reserved words, many of which occur in Conan-Doyle's text.
;; There are various ways you can browse the dictionary. To look at individual symbols:

(Doyle "baker")
;-> 10
(Doyle "street")
;-> 26

;; To look at the symbols as they are stored in a context, work through the context evaluating
;; each symbol, using dotree:

(dotree (wd Doyle)
(println wd { } (eval wd)))

;; To see the dictionary as an association list, use the dictionary name on its own. This creates
;; a new association list::

(Doyle)

;; This is a standard association list, which you can access using the functions described in the
;; Lists chapter (see Association lists2 ). For example, to ﬁnd all words that occur 20 times,
;; use ﬁnd-all:

(find-all '(? 20) (Doyle) (println $0))

;; The output will be:

;; baker
;; street


;; 7.5 Lists

;; Lists are a fundamental data structure in Lisp. They are ordered collections of elements,
;; separated by commas. Lists can contain elements of any type, including other lists.
;; newLISP provides several functions to manipulate lists:
;; • cons - constructs a new list by combining an element and a list
;; • car - returns the ﬁrst element of a list
;; • cdr - returns the rest of the elements of a list (after the �
;; • list - constructs a list from a variable number of arguments
;; • append - combines two lists into one
;; • reverse - reverses the order of elements in a list
;; • nth - returns the nth element of a list
;; • member - tests whether an element is a member of a list
;; • remove - removes an element from a list
;; • remove-duplicates - removes duplicate elements from a list
;; • map - applies a function to each element of a list
;; • filter - selects elements of a list based on a predicate
;; • fold - applies a function to each element of a list, combining the results
;; • sort - sorts a list in ascending order
;; • flatten - converts a nested list into a flat list
;; • random-element - selects a random element from a list
;; • random-sample - selects a random sample of elements from a list
;; • sublist - returns a sublist of a list
;; • take - returns the first n elements of a list
;; • drop - returns the elements after the first n elements of a list
;; • split-at - splits a list into two at a given index
;; • partition - splits a list into two lists based on a predicate
;; • group-by - groups elements of a list by a function
;; • intersperse - inserts a separator between elements of a list
;; • cycle - creates a cyclic list from a list


;; 7.5.1 Constructing a list

;; 7.5.2 Accessing elements of a list

;; 7.5.3 Modifying a list

;; 7.5.4 Using lists as a context

;; 7.5.5 Using lists as a function

;; The association list returned by (Doyle) is a temporary copy of the data in the dictionary,
;; not the original dictionary context. To change the data, don't operate on this temporary
;; list but on the context's data, using the key/value access techniques.
;; You can also add new entries to dictionaries, or modify existing entries, using data in the
;; form of an association list:

(Doyle '(("laser" 0) ("radar" 0)))

;; 7.5 Saving and loading contexts
;; If you want to use the dictionary again, you can save the context in a ﬁle:

(save "/Users/me/Sherlock Holmes/doyle-context.lsp" 'Doyle)

;; This collection of data, wrapped up in a context called Doyle, can be quickly loaded by
;; another script or newLISP session using:

(load "/Users/me/Sherlock Holmes/doyle-context.lsp")

;; and newLISP will automatically recreate all the symbols in the Doyle context, switching
;; back to the MAIN (default) context when done.


;; 7.6 Strings

;; Strings are sequences of characters, similar to lists. newLISP provides several functions
;; to manipulate strings:
;; • string - constructs a string from a variable number of arguments
;; • concat - concatenates two strings
;; • length - returns the length of a string
;; • substring - returns a substring of a string
;; • char - returns the character at a given index in a string
;; • char-code - returns the Unicode code point of a character
;; • char-name - returns the name of a character
;; • string-upcase - converts a string to uppercase
;; • string-downcase - converts a string to lowercase
;; • string-trim - removes leading and trailing whitespace from a string
;; • string-split - splits a string into a list of substrings
;; • string-join - joins a list of strings into a single string
;; • string-replace - replaces occurrences of a substring in a string
;; • string-index - returns the index of the first occurrence of a substring in a string
;; • string-rindex - returns the index of the last occurrence of a substring in a string
;; • string-match - tests whether a string matches a regular expression pattern
;; • string-format - formats a string using printf-style formatting
;; • string-capitalize - converts the ﬁrst character of a string to uppercase and
;; the rest to lowercase
;; • string-capitalize-words - converts the ﬁrst character of each word in
;; a string to uppercase and the rest to lowercase
;; • string-titlecase - converts the ﬁrst character of each word in a
;; string to uppercase, and the rest to lowercase, with the exception of certain
;; common words (such as articles, prepositions, and conjunctions)
;; • string-transcode - transcodes a string from one encoding to another
;; • string-encode-uri - encodes a string for use in a Uniform Resource Identifier
;; • string-decode-uri - decodes a string that has been encoded for use in a
;; Uniform Resource Identifier
;; • string-encode-base64 - encodes a string for use in Base64
;; • string-decode-base64 - decodes a string that has been encoded for use
;; in Base64
;; • string-encode-hex - encodes a string for use in hexadecimal
;; • string-decode-hex - decodes a string that has been encoded for use in
;; hexadecimal

;; 7.6.1 Constructing a string

;; 7.6.2 Accessing characters of a string

;; 7.6.3 Modifying a string

;; 7.6.4 Using strings as a context

;; 7.6.5 Using strings as a function

;; 7.6 Using newLISP modules

;; Contexts are used as containers for software modules because they provide lexically-
;; separated namespaces. The modules supplied with the newLISP installation usually deﬁne
;; a context that contains a set of functions handling tasks in a speciﬁc area.
;; Here's an example. The POP3 module lets you check POP3 email accounts. You ﬁrst load
;; the module:

(load "/usr/share/newlisp/modules/pop3.lsp")


;; Now you can use the functions provided by the POP3 module to interact with your email
;; accounts. For example, to connect to an email account and list all the messages in
;; the inbox:

(let ((server "pop.example.com")
      (username "your-username")
      (password "your-password"))
      (inbox (pop3-inbox server username password)))
      (dolist (message inbox)
      (println (pop3-message-subject message))))

;; 7.6.1 Constructing a string

;; 7.6.2 Accessing characters of a string

;; 7.6.3 Modifying a string

;; 7.6.4 Using strings as a context

;; 7.6.5 Using strings as a function

;; The module has now been added to the newLISP system. You can switch to the context:

(use-context POP3)

;; and use the functions provided by the POP3 module to interact with your email
;; accounts.


;; 7.7 The newLISP reader


;; 7.7.1 Reading and evaluating expressions

;; 7.7.2 Reading and evaluating statements

;; 7.7.3 Reading and evaluating comments

;; 7.7.4 Reading and evaluating multi-line strings

;; 7.7.5 Reading and evaluating character literals

;; and call the functions in the context. For example, to check your email, use the get-mail-
;; status function, supplying user name, password, and POP3 server name:

(get-mail-status "someone@example.com" "secret" "mail.example.com")
;-> (3 197465 37)
; (totalMessages, totalBytes, lastRead)


;; 7.8.1 Defining custom reader macros

;; 7.8.2 Defining custom syntax-rules

;; 7.8.3 Defining custom reader functions

;; 7.8.4 Defining custom syntax-case rules

;; 7.8.5 Defining custom syntax-error functions

;; If you don't switch to the context, you can still call the same function by supplying the full
;; address:

(POP3:get-mail-status "someone@example.com" "secret" "mail.example.com")

;; 7.7 Scoping

;; You've already seen the way newLISP dynamically ﬁnds the current version of a symbol
;; (see Scope3 ). However, when you use contexts, you can employ a diﬀerent approach, which
;; programmers call lexical scoping. With lexical scoping, you can explicitly control which
;; symbol is used, rather than rely on newLISP to keep track of similarly-named symbols for
;; you automatically.
;; In the following code, the width symbol is deﬁned inside the Right-just context.

(context 'Right-just)
(set 'width 30)
(define (Right-just:Right-just str)
(slice (string (dup " " width) str) (* width -1)))
(context MAIN)
(set 'width 0)
(dolist (w (symbols))
(println (Right-just w)))

;; The second (set 'width ...) line is a red herring: changing this here makes no diﬀerence at
;; all, because the symbol which is actually used by the right-justiﬁcation function is inside a
;; diﬀerent context.
;; You can still reach inside the Right-just context to set the width:


(context 'Right-just)
(set 'width 30)

;; 7.8.1 Defining custom reader macros

;; Reader macros allow you to extend the newLISP reader to interpret a custom syntax.
;; For example, the reader macro #+ allows you to include certain lines in your code only if
;; a certain condition is met.
;; Here's an example. Suppose you have a custom syntax for a simple calculator. You want


;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

(set 'Right-just:width 15)

;; There's been much discussion about the beneﬁts and disadvantages of the two approaches.
;; Whatever you choose, make sure you know where the symbols are going to get their values
;; from when the code runs. For example:

(define (f y)
        (+ y x))

;; Here, y is the ﬁrst argument to the function, and is independent of any other y. But what
;; about x? Is it a global symbol, or has the value been deﬁned in some other function that
;; has just called f? Or perhaps it has no value at all!
;; It's best to avoid using these free symbols, and to use local variables (deﬁned with let or
;; local) wherever possible. Perhaps you can adopt a convention such as putting asterisks
;; around a global symbol.

;; 7.8 Objects

;; More has been written about object-oriented programming (OOP) than you could possibly
;; read in one lifetime, so this section is just a quick glance at the subject. newLISP is agile
;; enough to enable more than one style of OOP, and you can easily ﬁnd references to these
;; on the web, together with discussions about the merits of each.
;; For this introduction, I'll brieﬂy outline just one of these styles: FOOP, or Functional
;; Object-Oriented Programming.


;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; 7.8.1 FOOP in a nutshell

;; FOOP has changed with newLISP version 10.2 (beginning of 2010), so if you're using and
;; old version of newLISP, please update it.
;; In FOOP, each object is stored as a list. Class methods and class properties (ie functions
;; and symbols that apply to every object of that class) are stored in a context.
;; Objects are stored in lists because lists are fundamental to newLISP. The ﬁrst item in an
;; object list is a symbol identifying the class of the object; the remaining items are the values
;; that describe the properties of an object.
;; All the objects in a class share the same properties but those properties can have diﬀerent
;; values. The class can also have properties that are shared between all objects in the class;
;; these are the class properties. Functions stored in the class context provide the various
;; methods for managing the objects and processing the data they hold.
;; To illustrate these ideas, consider the following code that works with times and dates. It
;; builds on top of the basic date and time functions provided by newLISP (see Working with
;; dates and times4 ). A moment in time is represented as a time object. An object holds


;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; two values: the number of seconds that have elapsed since the beginning of 1970, and the
;; time zone oﬀset, in minutes west of Greenwich. So the list to represent a typical time object
;; looks like this:

(Time 1219568914 0)

;; where Time is a symbol representing the class name, and the two numbers are the values of
;; this particular time (these numbers represent a time around 10 in the morning on Sunday
;; August 24 2008, somewhere in England).
;; The code required to build this object is straightforward using newLISP generic FOOP
;; constructor:

(new Class 'Time) ; defines Time context
     (setq some-england-date (Time 1219568914 0))


;; 7.8.2 Classical inheritance


;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; However, you might want to deﬁne a diﬀerent constructor, for example, you might want to
;; give this object some default values. To do that you have to redeﬁne the default function
;; which is acting as the constructor:

(define (Time:Time (t (date-value)) (zone 0))
(list Time t zone))


;; 7.8.3 Multiple inheritance

;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; Multiple inheritance is a feature that allows a class to inherit from more than one class.
;; For example, you might want to create a class called "Person" that inherits from both "
;; Human" and "Animal". Here's how you can do that:

(define (Person:Person (name age gender))
(list Person name age gender))

;; Now, you can create a new person object:

(let ((person (Person:Person "John" 30 "male")))
(println person)) ;-> (Person John 30 male)

;; 7.8.4 Polymorphism

;; 7.9.1 Debugging
;; 7.9.2 Logging

;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; It's a default function for the Time context that builds a list with the class name in the
;; ﬁrst position, and two more integers to represent the time. When values are not supplied,
;; they default to the current time and zero oﬀset. You can now use the constructor without
;; supplying some or any parameters:

(set 'time-now (Time))
;-> your output *will* differ for this one but will be something like (Time
1324034009 0)
(set 'my-birthday (Time (date-value 2024 4 30)))
;-> (Time 1211760000 0)
(set 'christmas-day (Time (date-value 2024 12 25)))
;-> (Time 1230163200 0)

;; 7.9.1 Debugging
;; 7.9.2 Logging
;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions

;; Next, you can deﬁne other functions to inspect and manage time objects. All these functions
;; live together in the context. They can extract the seconds and zone information by picking
;; them from the object by using the self function. So (self 1) gets the seconds and (self
;; 2) gets the time zone oﬀset from the object passed as parameter. Notice the deﬁnitions do
;; not require you to state the object parameter. Here are a few obvious class functions:

(define (Time:show)
       (date (self 1) (self 2)))

(define (Time:days-between other)
        "Return difference in days between two times."
        (div (abs (- (self 1) (other 1))) (* 24 60 60)))

(define (Time:get-hours)
        "Return hours."
        (int (date (self 1) (self 2) {%H})))

(define (Time:get-day)
        "Return day of week."
        (date (self 1) (self 2) {%A}))


;; 7.9.1 Debugging
;; 7.9.2 Logging
;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions
;; Now, you can use these functions on time objects:


;; 7.9.1 Debugging
;; 7.9.2 Logging
;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions
;; You can now use the newLISP debugger to step through the code and inspect the state of
;; your objects. The debugger will pause the execution of the code whenever a function is called
;; with a certain argument, and it will provide you with a prompt to inspect the state of the
;; objects. For example, you can add the following line to the end of your code:

;; (set 'time-now (Time))
;; (debugger)
;;
;; The debugger will pause the execution of your code at the point where it was called, and
;; you can then inspect the state of the objects.
;;

(define (Time:leap-year?)
        (let ((year (int (date (self 1) (self 2) {%Y}))))
        (and (= 0 (% year 4))
        (or (!= 0 (% year 100)) (= 0 (% year 400))))))


;; 7.9.1 Debugging
;; 7.9.2 Logging
;; 7.9.3 Tracing
;; 7.9.4 Profiling
;; 7.9.5 Assertions
;; Now, you can use the newLISP profiler to measure the execution time of your code.
;; First, you need to add the following line to the beginning of your code:

;; (profiler:start)
;;
;; Then, you can add the following line to the end of your code:
;; (profiler:stop)
;;
;; Finally, you can use the profiler to display the results:
;; (profiler:display)
;;

;; These functions are called by using the colon operator and providing the object which we
;; want the function to act upon:
; notice 'show' uses 'date' which works with local time, so your output probably
;; will differ

(:show christmas-day)
;-> Thu Dec 25 00:00:00 2008
(:show my-birthday)
;-> Mon May 26 01:00:00 2008

;; Notice how we used the colon operator as a preﬁx to the function, without separating it
;; with a space, this is a matter of style, you can use it with ot whitout spaces:

(:show christmas-day) ; same as before
;-> Thu Dec 25 00:00:00 2008
(: show christmas-day) ; notice the space between colon and function
;-> Thu Dec 25 00:00:00 2008

;; This technique allows newLISP to provide a feature that object-oriented programmers love:
;; polymorphism.

;; 7.8.2 Polymorphism
;; Let's add another class which works on durations - the gap between two time objects -
;; measured in days.

(define (Duration:Duration (d 0))
        (list Duration d))
        (define (Duration:show)
        (string (self 1) " days "))

;; There's a new class constructor Duration:Duration for making new duration objects, and a
;; simple show function. They can be used with the time objects like this:

; define two times
(set 'time-now (Time) 'christmas-day (Time (date-value 2008 12 25)))
; show days between them using the Time:days-between function
(:show (Duration (:days-between time-now christmas-day)))
;-> "122.1331713 days "


;; 7.8.3 Multiple inheritance
;; Let's create a new class "Employee" that inherits from both "Person" and "Duration
;; (which represents a gap between two time objects).

(define (Employee:Employee (name age gender hire-date duration))
        (list Employee name age gender hire-date duration))
        (define (Employee:show)
        (string (self 1) " (" (self 3) ") " (self 2
        " years old, hired on " (format-time-string "%A, %B %
         %d, %Y" (self 4)) ", worked " (self 5) "
         days)"))
         (define (Employee:worked)
         (:days-between (self 4) (self 1))))
         (define (Employee:get-hire-date)
         (format-time-string "%A, %B %d, %Y" (self
         (list 4))))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))
         (define (Employee:get-age)
         (self 2))
         (define (Employee:get-gender)
         (self 3)))
         (define (Employee:get-working-days)
         (self 5)))
         (define (Employee:get-hire-date)
         (self 4)))
         (define (Employee:get-duration)
         (self 5)))
         (define (Employee:get-name)
         (self 1))

         ;; and so on...

         