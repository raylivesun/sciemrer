;; 7.2 Contexts: the basics
;; The context function can be used for a number of diﬀerent tasks:
;; • to create a new context
;; • to switch from one context to another
;; • to retrieve the value of an existing symbol in a context
;; • to see what context you're in
;; • to create a new symbol in a context and assign a value to it
;; newLISP can usually read your mind, and knows what you want to do, depending on how
;; you use the context function. For example:

(context 'Test)

  ;; (set 'x 10)
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (print x)  ; prints 0
  ;; (set 'x 20)
  ;; (print x)  ; prints 20
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (set 'y 30)
  ;; (print y)  ; prints 30
  ;; (context 'Main)
  ;; (print y)  ; prints 0
  ;; (context 'Test)
  ;; (set 'y 40)
  ;; (print y)  ; prints 40
  ;; (context 'Main)
  ;; (print y)  ; prints 30
  ;; (context 'Test)
  ;; (set 'x 50)
  ;; (print x)  ; prints 50
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (set 'z 60)
  ;; (print z)  ; prints 60
  ;; (context 'Main)
  ;; (print z)  ; prints 0
  ;; (context 'Test)
  ;; (set 'z 70)
  ;; (print z)  ; prints 70
  ;; (context 'Main)
  ;; (print z)  ; prints 60
  ;; (context 'Test)
  ;; (set 'z 80)

  ;; (context 'Test)
  ;; (let ((x 90))
  ;;   (print x)  ; prints 90
  ;;   (context 'Main)
  ;;   (print x)  ; prints 0
  ;; )
  ;; (context 'Test)
  ;; (print x)  ; prints 90
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (set 'x 100)
  ;; (print x)  ; prints 100
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (let ((x 110))
  ;;   (print x)  ; prints 110
  ;;   (context 'Main)
  ;;   (print x)  ; prints 0
  ;; )
  ;; (context 'Test)
  ;; (print x)  ; prints 110
  ;; (context 'Main)
  ;; (print x)  ; prints 10
  ;; (context 'Test)
  ;; (set 'x 120)
  ;; (print x)  ; prints 120
  ;; (context 'Main)

  ;; (context 'Test)
  ;; (let ((x 130))
  ;;   (print x)  ; prints 130
  ;;   (context 'Main)
  ;;   (print x)  ; prints 0
  ;; )
  ;; (context 'Test)

  ;; (context 'Test)
  ;; (let ((x 140))
  ;;   (print x)  ; prints 140
  ;;   (context 'Main)
  ;;   (print x)  ; prints 0
  ;; )
  ;; (context 'Test)
  ;; (print x)  ; prints 140
  ;; (context 'Main)

;; And you can switch between contexts freely:
(context MAIN)
;; MAIN
(context 'Test)
;; Test
;; Test>
;; Used on its own, it just tells you where you are:
> (context)
;; MAIN

;; Once a context exists, you don't have to quote the name (but you can if you like). Notice
;; that I've used an upper-case letter for my context name. This is not compulsory, just a
;; convention.

;; A context contains symbols and their values. There are various ways to create a symbol
;; and give it a value.


;; 7.3 Creating and assigning symbols
;; Here are some examples:


;; (set 'x 10)
;; (set 'y 20)
;; (set 'z 30)


;; You can also use setq to create a symbol and give it a value:

;; (setq x 10)
;; (setq y 20)
;; (setq z 30)

;; 7.4 Using symbols
;; You can use a symbol to get the value it holds:

;; (print x)  ; prints 10
;; (print y)  ; prints 20
;; (print z)  ; prints 30

;; You can also use the symbol as a variable:
(context 'Doyle "villain" "moriarty")
"moriarty"

;; This creates a new context - notice the quote, because newLISP hasn't seen this before
;; - and a new symbol called "villain", with a value of "Moriarty", but stays in the MAIN
;; context. If the context already exists, you can omit the quote:

(context Doyle "hero" "holmes")
"holmes"

;; 7.5 Contexts and symbols
;; Contexts are like namespaces for symbols. When you create a new symbol, it goes into the
;; current context. If you switch to a different context, all symbols created in that
;; context are lost.

;; (context 'Test)
;; (set 'x 10)
;; (context 'Main)
;; (print x)  ; prints 0

;; 7.6 Symbol values
;; Symbol values can be any Lisp object, including numbers, strings, lists, and even other
;; Lisp objects.

;; (set 'x 10)
;; (set 'y "hello")
;; (set 'z '(1 2 3))

;; You can use the symbol as a variable to get its value:

;; (print x)  ; prints 10
;; (print y)  ; prints "hello"
;; (print z)  ; prints (1 2 3)


;; 7.7 Symbol lookup
;; When you use a symbol, newLISP looks up the value in the current context, and
;; then in the parent context, and so on, until it finds the symbol or reaches the MAIN
;; context. If the symbol is not found in any context, newLISP throws an error.

;; (context 'Test)
;; (set 'x 10)
;; (context 'Main)
;; (print x)  ; prints 10
;; (context 'Test)
;; (print x)  ; prints 10
;; (context 'Doyle "villain" "moriarty")
;; (print x)  ; prints 10

;; (context 'Test)
;; (set 'y 20)
;; (context 'Main)
;; (print y)  ; prints 20
;; (context 'Test)
;; (print y)  ; prints 20

;; To obtain the value of a symbol, you can do this:
(context Doyle "hero")
"holmes"

;; To obtain the value of a symbol in a different context, you can do this:
(context 'Test)
(context 'Doyle "hero")
"holmes"

;; or, if you're using the console, this step by step approach:
(context 'Doyle)
Doyle
Doyle> hero
"holmes"
Doyle>

;; The full address of a symbol is the context name, followed by a colon (:), followed by the
;; symbol name. Always use the full address if you're in another context.


;; 7.8 Symbol properties
;; Lisp provides a way to associate properties with symbols. Properties are key-value


;; 7.9 Symbol renaming
;; You can rename a symbol by creating a new symbol with the same name in the current
;; context, and then giving it the same value as the original symbol.

;; (context 'Test)
;; (set 'x 10)
;; (context 'Main)
;; (print x)  ; prints 10
;; (context 'Test)
;; (set 'y 10)
;; (context 'Main)
;; (print y)  ; prints 10
;; (set 'x y)
;; (context 'Test)

;; To see all the symbols inside a context, use symbols to produce a list:
(symbols Doyle)
;-> (Doyle:hero Doyle:period Doyle:villain)
;; or, if you're already inside the Doyle context:
(symbols)
;-> (hero period villain)
;; You can use this list of symbols in the usual way, such as stepping through it with dolist.
(dolist (s (symbols Doyle))
(println s))


;; 7.10 Symbol deletion
;; You can delete a symbol using the unintern function. However, this will not affect
;; any references to the symbol.

;; (context 'Test)
;; (set 'x 10)
;; (context 'Main)
;; (print x)  ; prints 10
;; (context 'Test)
;; (set 'y 10)
;; (context 'Main)

;; To see the values of each symbol, use eval to ﬁnd its value, and term to return just the
;; symbol's name.

(dolist (s (symbols Doyle))
(println (term s) " is " (eval s)))

;; There's a more eﬃcient (slightly faster) technique for looping through symbols in a context.
;; Use the dotree function:

(dotree (s Doyle)
(println (term s) " is " (eval s)))


;; 7.2.1 Creating contexts implicitly
;; As well as explicitly creating contexts with context, you can have newLISP create contexts
;; for you automatically. For example:


;; (set 'x 10)
;; (set 'y 20)
;; (set 'z 30)


;; You can also use the make-context function to create a new context:

;; (context 'Doyle "villain" "moriarty")

;; 7.2.2 Using contexts implicitly
;; When you use a symbol, newLISP looks up the value in the current context, and
;; then in the parent context, and so on, until it finds the symbol or reaches the MAIN

(define (C:greeting)
(println "greetings from context " (context)))
(C:greeting)

;; Here, newLISP has created a new context C and a function called greeting in that context.
;; You can create symbols this way too:

(define D:greeting "this is the greeting string of context D")
(println D:greeting)


;; 7.2.3 Context lookup implicitly
;; When you use a symbol, newLISP looks up the value in the current context, and
;; then in the parent context, and so on, until it finds the symbol or reaches the MAIN

(context 'Test)
(set 'x 10)
(context 'Main)
(print x)  ; prints 10
(context 'Test)
(print x)  ; prints 10
(context 'Doyle "villain" "moriarty")
(print x)  ; prints 10

;; 7.2.4 Symbol lookup implicitly
;; When you use a symbol, newLISP looks up the value in the current context, and
;; then in the parent context, and so on, until it finds the symbol or reaches the MAIN

(define (C:greeting)
(println "greetings from context " (context)))
(C:greeting)


;; In both these examples, notice that you stayed in the MAIN context.
;; The following code creates a new context L containing a new list called ls that contains
;; strings:

(set 'L:ls '("this" "is" "a" "list" "of" "strings"))
;-> ("this" "is" "a" "list" "of" "strings")

;; Then, it creates a new context D containing a symbol called greeting that holds a
;; string:

(define D:greeting "this is the greeting string of context D")
;-> "this is the greeting string of context D"

;; Finally, it creates a new context E containing a function called f that prints the
;; contents of the list ls and the value of the symbol greeting:

(define E:f (lambda ()
(println "list contents: " L:ls)
(println "greeting: " D:greeting)))
(E:f)

;; This output will be:
;; list contents: ("this" "is" "a" "list" "of" "

   ˘
;-> greeting: this is the greeting string of context D

;; 7.2.5 Context lookup implicitly
;; When you use a symbol, newLISP looks up the value in the current context, and
;; then in the parent context, and so on, until it finds the symbol or reaches the MAIN

;; 7.3.1 Defining functions
;; A function is a named block of code that takes zero or more arguments, executes the
;; code inside the block, and returns a value.
;; To define a function, use the define function:

;; (define (add x y)
;; (+ x y))

;; Now, you can call the function:

;; (add 5 7)  ; prints 12

;; 7.3.2 Function arguments
;; You can have optional arguments by using default values for the arguments. For
;; example:

;; (define (add x y z)
;; (+ x y z))
;; (add 5 7)  ; prints 12
;; (add 5 7 3)  ; prints 15

;; 7.3.3 Function return values
;; A function can return a value using the return function:

;; (define (add x y)
;; (+ x y))
;; (define (subtract x y)
;; (- x y))
;; (define (multiply x y)
;; (* x y))
;; (define (divide x y)
;; (/ x y))

;; Now, you can call the functions:

;; (add 5 7)  ; prints 12
;; (subtract 10 5)  ; prints 5
;; (multiply 5 7)  ; prints 35
;; (divide 10 2)  ; prints 5

;; 7.3.4 Function call expressions
;; You can call a function with arguments using the function call expression:

;; (add 5 7)  ; prints 12

;; 7.3.5 Function recursion
;; You can define a recursive function by using the lambda function:

;; (define (factorial n)
;; (if (= n 0)
;;     1
;;     (* n (factorial (- n 1)))))
;; (factorial 5)  ; prints 120

;; 7.3.6 Function closures
;; A function can close over variables from its surrounding scope. This means that the
;; function can access and modify the variables from its surrounding scope even after the
;; surrounding scope has finished executing. For example:

;; (define (make-adder x)
;; (lambda (y)
;; (+ x y)))
;; (define adder1 (make-adder 5))

;; Functions in context
;; Contexts can contain functions and symbols. To create a function in a context other than
;; MAIN, either do this:

(context Doyle)
(define (hello-world)
(println "Hello World"))


;; or, if you're already inside the Doyle context:

(define Doyle:hello-world (lambda ()
(println "Hello World")))


;; or do this

(context MAIN)
(define (Doyle:hello-world)
(println "Hello World"))


;; 7.3.7 Function call expressions
;; You can call a function with arguments using the function call expression:

;; (Doyle:hello-world)

;; 7.3.8 Function closures
;; A function can close over variables from its surrounding scope. This means that the
;; function can access and modify the variables from its surrounding scope even after the


;; 7.4.1 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.2 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.3 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.4 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.5 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.6 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.7 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.8 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.9 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.10 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.11 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.12 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.13 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.14 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.15 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.16 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.17 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.18 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.19 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.20 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.21 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.22 Symbol properties 
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.23 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.24 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.25 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.26 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.27 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.28 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.29 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.30 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.31 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.32 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.33 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.34 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.35 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.36 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.37 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.38 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.39 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.40 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.41 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.42 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.43 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159


;; 7.4.44 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159


;; 7.4.45 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.46 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)    
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; 7.4.47 Symbol properties
;; You can define properties for symbols using the defvar function:

;; (defvar *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You can also define properties for symbols using the defparameter function:

;; (defparameter *pi* 3.14159)
;; (symbol-value *pi*)  ; prints 3.14159

;; You don't have to quote the new context name here because we're using deﬁne, and deﬁne
;; (by deﬁnition) isn't expecting the name of an existing symbol.
;; To use functions while you're in another context, remember to call them using this con-
;; text:function syntax.

;; 7.3 The default function

;; If a symbol in a context has the same name as the context, it's known as the default function
;; (although in fact it can be either a function, or a symbol containing a list or a string). For
;; example, here is a context called Evens, and it contains a symbol called Evens:

(define Evens:Evens (sequence 0 30 2))
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
Evens:Evens
;-> (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)

;; And here is a function called Double in a context called Double:

(define (Double:Double x)
(mul x 2))

;; So Evens and Double are the default functions for their contexts.
;; There are lots of good things about default functions. If the default function has the same
;; name as the context, it is evaluated whenever you use the name of the context in expressions,
;; unless newLISP is expecting the name of a context. For example, although you can always
;; switch to the Evens context by using the context function in the usual way:

(context 'Evens)
;; Evens
;; Evens> (context MAIN)
;; MAIN

;; you can use Evens as a list (because Evens:Evens is a list):

(reverse Evens)
;-> (30 28 26 24 22 20 18 16 14 12 10 8 6 4 2 0)

;; You can use the default function without supplying its full address. Similarly, you can
;; use the Double function as an ordinary function without supplying the full colon-separated
;; address:

(Double 3)








