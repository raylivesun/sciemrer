;; Compare that :show function call with the :show in the previous section:
(:show christmas-day)
;-> Thu Dec 25 00:00:00 2008


;; Use :as to assign a new name to a value:
(defalias my-christmas-day 'christmas-day)
(:show my-christmas-day)
;-> Thu Dec 25 00:00:00 200

;; You can see that newLISP is choosing which version of the show function to evaluate,
;; according to the class of :shows parameter. Becausechristmas-dayis a Time object,
;; newLISP evaluatesTime:show. But when the argument is a Duration object, it


;; Use :type to check the type of a value:
(:type my-christmas-day)
;-> java.time.LocalDate

;; Use :doc to get documentation for a function:
(:doc time:show)
;-> (show object)
;     Show the object as a string.
;
;     If the object is a Time object, it shows the time in the format
;     "yyyy-MM-dd HH:mm:ss.SSS".
;
;     If the object is a Duration object, it shows the duration in the format
;     "PnYnMnDTnHnMnS".


;; Use :load to load a file containing Lisp code:
(load "my-code.lisp")
;-> nil

;; Use :require to load a file containing Lisp code, and automatically
;; import any necessary functions:
(require :my-module)
;-> nil


;; Use :eval to evaluate a string containing Lisp code:
(:eval '(+ 1 2))
;-> 3

;; Use :quit to exit newLISP:
(:quit)
;-> nil

;; evaluatesDuration:show. The idea is that you can use a function on various
;; types of object: you perhaps don't need to know what class of object you're
;; dealing with. With this polymorphism, you can apply theshowfunction to a
;; list of objects of diﬀering types, and newLISP selects the appropriate one each
;; time:

(map (curry :show)
(list my-birthday (Duration (:days-between time-now christmas-day))))

;; Note: We have to use curry here because map needs to use both the colon operator as
;; well as the show function.

;; 7.8.3 Modifying objects

;; We're calling this particular style of OOP FOOP because it's considered to be functional.
;; Here, the term 'functional' refers to the style of programming encouraged by newLISP which
;; emphasizes the evaluation of functions, avoiding state and mutable data. As you've seen,
;; many newLISP functions return copies of lists rather than modify the originals. There are,
;; though, a small number of functions that are called destructive, and these are considered
;; to be less purely functional. But FOOP doesn't provide for destructive object methods, so
;; can be considered more functional.
;; A key point to notice about FOOP is that objects are immutable; they can't be modiﬁed by
;; class functions. For example, here's a function for the Time class that adds a given number
;; of days to a time object:


;; 7.8.4 Implementing new classes


;; 7.8.5 Summary

;-> nil


;; 7.8.6 Exercises

;-> nil


;; 7.8.7 Additional reading

;-> nil

;-> nil

;; 7.8.8 Quiz

;-> nil

(define (Time:adjust-days number-of-days)
(list Time (+ (* 24 60 60 number-of-days) (self 1)) (self 2)))

;; The original date of the christmas-day object didn't change, although the :adjust-days
;; function returned a modiﬁed copy adjusted by 3 days.
;; In other words, to make changes to objects, use the familiar newLISP approach of using
;; the value returned by a function:

(set 'christmas-day (:adjust-days christmas-day 3))

(:show christmas-day)
;-> "Sun Dec 28 00:00:00 2008"


;; 7.9.1 Introduction

;-> nil

;; 7.9.2 Immutable data

;-> nil


;; 7.9.3 Mutable data

;-> nil

;; 7.9.4 Summary

;-> nil

;; christmas-day now contains the modiﬁed date.
;; You can ﬁnd a more complete working out of this idea by searching the newLISP forum for
;; timeutilities. Also make sure to read the section on FOOP in the reference manual5 , it
;; has a nice example on nested objects, that is, objects containing other objects.


;; 7.9.5 Additional reading

;-> nil

;-> nil

;; 7.9.6 Quiz

;-> nil

(defparameter *time-now* (Time:now))

;; The time-now variable now contains the current time.
;; 7.9.7 Exercises

;-> nil

;; 7.9.8 Additional reading

;-> nil

;-> nil

;; 7.9.9 Quiz

;-> nil

(defparameter *time-now* (Time:now))

;; The time-now variable now contains the current time.
;; 7.10.1 Introduction

;-> nil

;; 7.10.2 Strings

;-> nil

;; 8 Macros
;; 8.1 Introducing macros

;; We've covered the basics of newLISP, but there are plenty of powerful features left to
;; discover. Once you've grasped the main rules of the language, you can decide which of
;; the more advanced tools you want to add. One feature that you might want to explore is
;; newLISP's provision of macros.
;; A macro is a special type of function that you can use to change the way your code is
;; evaluated by newLISP. For example, you can create new types of control functions, such as
;; your own version of if or case.
;; With macros, you can start to make newLISP work exactly the way you want it to.
;; Strictly speaking, newLISP's macros are fexprs, not macros. In newLISP, fexprs are called
;; macros partly because it's much easier to say 'macros' than 'fexprs' but mainly because
;; they serve a similar purpose to macros in other LISP dialects: they allow you to deﬁne
;; special forms such as your own control functions.


;; 8.2 Defining a new macro


;; 8.3 The :defmacro macro

;; 8.4 Using a macro

;; 8.5 Summary

;-> nil

;; 8.5.1 Additional reading

;-> nil

;; 8.2 When do things get evaluated

;; To understand macros, let's jump back to one of the very ﬁrst examples in this introduction.
;; Consider the way this expression is evaluated:

(* (+ 1 2) (+ 3 4))
;-> (* 3 7)
;-> 21


;; The expression is evaluated from left to right, as you've probably learned. When the
;; multiplication operator (*) is encountered, it's evaluated with the arguments 3 and 7.
;; The result of the multiplication is then used as the argument to the addition operator (+).
;; Finally, the result of the addition is used as the result of the entire expression.
;; This is a simple example of how macros can be used to manipulate the evaluation of
;; expressions.
;; Now let's look at an example that demonstrates the use of a macro. We'll create a
;; macro called :double that takes an argument and returns the argument multiplied by 2:

(defmacro :double (x) (* x 2))

;; Now let's try using the :double macro:

(:double 3)

;; The * function doesn't see the + expressions at all, only their results. newLISP has
;; enthusiastically evaluated the addition expressions and then handed just the results to
;; the multiplication function. This is usually what you want, but there are times when you
;; don't want every expression evaluated immediately.
;; Consider the operation of the built-in function if:

(if (<= x 0) (exit))


;; The if function evaluates its arguments immediately, and if the result of the
;; comparison is true, it evaluates the first argument (the condition) and exits the
;; program. If the result of the comparison is false, it evaluates the second argument
;; (the then-clause) and continues with the rest of the program.
;; If you want to use the if function to exit the program when a condition is true,
;; you can't use it directly. Instead, you can use the :exit macro, which takes
;; a single argument and causes newLISP to exit with that argument as the exit status.
;; Here's how you can use the :exit macro:


;; 8.5.2 Quiz

;-> nil

(defmacro :double (x) (* x 2))

;; The :double macro has been defined. Now let's try using it:

(:double 3)

;; If x is greater than 0, the test returns nil so the (exit) function isn't evaluated. Now
;; suppose that you want to deﬁne your own version of the if function. It ought to be easy:

(define (my-if test true-action false-action)
        (if test true-action false-action))

(my-if (> 3 2) (println "yes it is" ) (exit))


;; Now let's try this version of the if function:

;; But this doesn't work. If the comparison returns true, newLISP prints a message and then
;; exits. Even if the comparison returned false, newLISP would still exit, without printing
;; a message. The problem is that (exit) is evaluated before the my-if function is called,
;; even when you don't want it to be. For ordinary functions, expressions in arguments are
;; evaluated ﬁrst.

;; Macros are similar to functions, but they let you control when - and if - arguments are
;; evaluated. You use the deﬁne-macro function to deﬁne macros, in the same way that
;; you deﬁne functions using deﬁne. Both these deﬁning functions let you make your own
;; functions that accept arguments. The important diﬀerence is that, with the ordinary deﬁne,
;; arguments are evaluated before the function runs. But when you call a macro function
;; deﬁned with deﬁne-macro, the arguments are passed to the deﬁnition in their raw and
;; unevaluated form. You decide when the arguments are evaluated.
;; A macro version of the my-if function looks like this:

(define-macro (my-if test true-action false-action)
             (if (eval test) (eval true-action) (eval false-action)))

(my-if (> 3 2) (println "yes it is" ) (exit))

;; Now let's try this version of the if function:

;; The test and action arguments aren't evaluated immediately, only when you want them to
;; be, using eval. And this means that the (exit) isn't evaluated before the test has been
;; made.

;; This ability to postpone evaluation gives you the ability to write your own control structures
;; and add powerful new forms to the language.


;; 8.6 Summary

;-> nil

;; 8.6.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; 8.7.1 Additional reading

;-> nil

;; 8.7.2 Quiz

;-> nil

(defmacro :double (x) (* x 2))

;; The :double macro has been defined. Now let's try using it:

(:double 3)

;; 8.3 Tools for building macros

;; newLISP provides a number of useful tools for building macros. As well as deﬁne-macro
;; and eval, there's letex, which gives you a way of expanding local symbols into an expression
;; before evaluating it, and args, which returns all the arguments that are passed to your
;; macro.


;; 8.3.1 The letex macro

;; The letex macro is used to expand local symbols into an expression before evaluating it.
;; For example:

(letex (x 3) (* x 2))


;; The letex macro expands x into 3 before evaluating the multiplication expression.
;; The result of the multiplication is 6.
;; 8.3.2 The args macro

;; The args macro returns all the arguments that are passed to your macro. For example:

(define-macro (my-macro arg1 arg2)
         (list 'my-macro arg1 arg2))
         (my-macro 3 4)


;; The args macro returns (my-macro 3 4).
;; 8.3.3 The letrec macro

;; The letrec macro is used to create local bindings and evaluate their expressions in a
;; single step. For example:

(letrec ((x 3)
         (square (lambda (y) (* y y))))
         (* x square))
         (* 3 (square 2))

;; 8.4 Symbol confusion

;; One problem to be aware of when you're writing macros is the way that symbol names
;; in macros can be confused with symbol names in the code that calls the macro. Here's a
;; simple macro which adds a new looping construct to the language that combines dolist and
;; do-while. A loop variable steps through a list while a condition is true:


;; 8.4.1 The do-while macro

;; The do-while macro is used to create a looping construct that steps through a list
;; while a condition is true. The loop variable steps through the list, and the
;; condition is evaluated after each step. Here's a simple macro which adds a new
;; looping construct to the language that combines dolist and do-while:

(define-macro (do-while var list condition action)
         (let ((var (car list))
         (list 'do-while var (cdr list) condition (cons action (cdr (cdr list
          (cons var list)))))
         (list var (car list)))
         (eval (car list)))
         (do-while var (cdr list) condition action))
         (exit)


;; 8.4.2 Using the do-while macro


;; 8.5 Summary

;-> nil

;; 8.5.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; 8.7.1 Additional reading

;-> nil

;; 8.7.2 Quiz

;-> nil

(defmacro :double (x) (* x 2))

;; The :double macro has been defined. Now let's try using it:

(:double 3)

;; 8.3 Tools for building macros

(define-macro (dolist-while)
        (letex (var (args 0 0)
        ; loop variable
        lst (args 0 1)
        ; list
        cnd (args 0 2)
        ; condition
        body (cons 'begin (1 (args)))) ; body
        (let (y)
             (catch (dolist (var lst)
        (if (set 'y cnd) body (throw y)))))))

;; It's called like this:
(dolist-while (x (sequence 20 0) (> x 10))
    (println {x is } (dec x 1)))


;; And it appears to work well. But there's a subtle problem: you can't use a symbol called y
;; as the loop variable, even though you can use x or anything else. Put a (println y) statement
;; in the loop to see why:

(dolist-while (x (sequence 20 0) (> x 10))
    (println {x is } (dec x 1))
    (println {y is } y))


;; If you try to use y, it won't work:
(dolist-while (y (sequence 20 0) (> y 10))
    (println {y is } (dec y 1)))
;; y is
;; value expected in function dec : y

;; The problem is that y is used by the macro to hold the condition value, even though it's
;; in its own let expression. It appears as a true/nil value, so it can't be decremented. To ﬁx
;; this problem, enclose the macro inside a context, and make the macro the default function
;; in that context:

(context 'dolist-while)
       (define-macro (dolist-while:dolist-while)
       (letex (var (args 0 0)
       lst (args 0 1)
       cnd (args 0 2)
       body (cons 'begin (1 (args))))
       (let (y)
            (catch (dolist (var lst)
       (if (set 'y cnd) body (throw y)))))))

(context MAIN)


;; Now you can use y as the loop variable:

;; This can be used in the same way, but without any problems:
(dolist-while (y (sequence 20 0) (> y 10))
    (println {y is } (dec y 1)))


;; 8.5 Summary

;-> nil

;; 8.5.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; Other ideas for macros
;; newLISP users ﬁnd many diﬀerent reasons to use macros. Here are a couple of macro
;; deﬁnitions I've found on the newLISP user forums.
;; Here's a version of case, called ecase (evaluated-case) that really does evaluate the tests:

(define-macro (ecase _v)
       (eval (append
       (list 'case _v)
       (map (fn (_i) (cons (eval (_i 0)) (rest _i)))
       (args)))))
(define (test n)
        (ecase n
        ((/ 4 4)
        (println "n was 1"))
((- 12 10)
 (println "n was 2"))))
(set 'n 2)
(test n)


;; Here's a macro for string concatenation that works with nested expressions:

(define-macro (concat-string . args)
       (let ((result '()))
       (dolist (arg args)
       (if (stringp arg)
       (set! result (append result (list 'string arg)))
       (set! result (append result (list 'quote arg))))))
       (list 'string result))


;; Both of these macros can be used like this:

(concat-string "Hello " "world!")

;; 8.6 Summary

;-> nil

;; 8.6.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; You can see that the divisions (/ 4 4) and (- 12 10) were both evaluated. They wouldn't
;; have been with the standard version of case.
;; Here's a macro that creates functions:

(define-macro (create-functions group-name)
       (letex
       ((f1 (sym (append (term group-name) "1")))
        (f2 (sym (append (term group-name) "2"))))
       (define (f1 arg) (+ arg 1))
       (define (f2 arg) (+ arg 2))))

(create-functions foo)
; this creates two functions starting with 'foo'

(foo1 10)
;-> 11
(foo2 10)
;-> 12

(create-functions bar)
; and this creates two functions starting with 'bar'
(bar1 12)
;-> 13
(bar2 12)
;-> 14

;; 8.6 Summary

;-> nil

;; 8.6.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; 8.4.1 A tracer macro
;; The following code changes the operation of newLISP so that every function deﬁned using
;; deﬁne will, when evaluated, add its name and details of its arguments to a log ﬁle. When
;; you run a script, the log ﬁle will contain a record of the functions and arguments that were
;; evaluated.

(context 'tracer)
    (define-macro (tracer:tracer farg)
    (set (farg 0)
         (letex (func
                  (farg 0)
                arg
                (rest farg)
          arg-p (cons 'list (map (fn (x) (if (list? x) (first x) x))
                                 (rest farg)))
          body
                (cons 'begin (args)))
         (lambda
         arg
         (append-file
               (string (env "HOME") "/trace.log")
               (string 'func { } arg-p "\n"))
         body))))

(context MAIN)
(constant (global 'newLISP-define) define)
; redefine the built-in define:
(constant (global 'define) tracer)

;; To run a script with this simple tracer, load the context before you run:

(load {tracer.lsp})

;; The log ﬁle generated contains a list of every function that was called, and the arguments
;; it received:

;; Time:Time (1211760000 0)
;; Time:Time (1230163200 0)
;; Time:Time (1219686599 0)
;; show ((Time 1211760000 0))
;; show ((Time 1230163200 0))
;; get-hours ((Time 1219686599 0))
;; get-day ((Time 1219686599 0))
;; days-between ((Time 1219686599 0) (Time 1230163200 0))
;; leap-year? ((Time 1211760000 0))
;; adjust-days ((Time 1230163200 0) 3)
;; show ((Time 1230422400 0))
;; Time:Time (1219686599 0)
;; days-between ((Time 1219686599 0) (Time 1230422400 0))
;; Duration:Duration (124.256956)
;; period-to-string ((Duration 124.256956))
;; days-between ((Time 1219686599 0) (Time 1230422400 0))
;; Duration:Duration (124.256956)
;; Time:print ((Time 1211760000 0))
;; Time:string ((Time 1211760000 0))
;; Duration:print ((Duration 124.256956))
;; Duration:string ((Duration 124.256956))


;; 8.5.1 Additional reading

;-> nil

;; 8.7 Exercises

;-> nil

;; It will slow execution quite a lot.
;; The following code changes the operation of newLISP so that every function deﬁned
;; using deﬁne will, when evaluated, add its name and details of its arguments
;; to a log ﬁle. When you run a script, the log �
;; file will contain a record of the functions and arguments that were evaluated.

(context 'tracer)
    (define-macro (tracer:tracer farg)
    (set (farg 0)
         (letex (func
          (farg 0)
                arg
                (rest farg)
                arg-p (cons 'list (map (fn (x) (if (list? x
                (first x)
                x))
                                (rest farg)))
                                body
                                (cons 'begin (args)))
                                 (lambda
                                 arg
                                 (append-file
                                 (string (env "HOME") "/trace.log")
                                 (string 'func { } arg-p "\n"))
                                 body))))
                                 (context MAIN)
                                 (constant (global 'newLISP-define) define)

    ; redefine the built-in define:
    (constant (global 'define) tracer)

    ;; To run a script with this simple tracer, load the context before you run:
    (load {tracer.lsp})
    ;; The log ﬁle generated contains a list of every function that was called, and
    ;; the arguments it received:
    ;; Time:Time (1211760000 0)
    ;; Time:Time (1230163200 0)
    ;; Time:Time (1219686599 0)
    ;; show ((Time 1211760000 0))
    ;; show ((Time 1230163200 0))
    ;; get-hours ((Time 1219686599 0))
    ;; get-day ((Time 1219686599 0))
    ;; days-between ((Time 1219686599 0)
    ;; (Time 1230163200 0))
    ;; leap-year? ((Time 1211760000
    ;; 0))
    ;; adjust-days ((Time 1230163200 0)

;; It will slow execution quite a lot.

;; 9 Working with numbers

;; If you work with numbers, you'll be pleased to know that newLISP includes most of the
;; basic functions that you'd expect to ﬁnd, plus many more. This section is designed to help
;; you make the best use of them and to avoid some of the minor pitfalls that you might
;; encounter. As always, see the oﬃcial documentation for full details.

;; 9.1 Integers and ﬂoating-point numbers

;; newLISP handles two diﬀerent types of number: the integer and the ﬂoating-point number.
;; Integers are precise, whereas ﬂoating-point numbers (ﬂoats) are less precise. There are
;; advantages and disadvantages to each. If you need to use very large integers, larger than

;; 9 223 372 036 854 775 807, see the section covering the diﬀerences between large (64-bit)
;; integers and big integers (of unlimited size) - Bigger numbers1 .
;; The arithmetic operators +, -, *, /, and % always return integer values. A common
;; mistake is to forget this and use / and * without realising that they're carrying out integer
;; arithmetic:

(/ 10 3)
; -> 3

(* 10 3)
; -> 30


;; 9.2 The modulo operator

;; The modulo operator (%) is used to calculate the remainder when one number is divided by
;; another. It returns a number that is always less than the second number:

(mod 10 3)
; -> 1

;; 9.3 The quotient and remainder functions

;; 9.3.1 quotient


;; The quotient function (quotient) is used to calculate the integer division result of
;; two numbers. It returns the largest integer that is less than or equal to the result of
;; dividing the two numbers:

(quotient 10 3)
; -> 3

;; 9.3.2 remainder

;; The remainder function (remainder) is used to calculate the remainder when one number
;; is divided by another. It returns a number that is always less than the second number:

(remainder 10 3)
; -> 1

;; 9.4 The integer division operator

;; The integer division operator (/) is used to calculate the integer division result of
;; two numbers. It returns the largest integer that is less than or equal to the result of
;; dividing the two numbers:

(/ 10 3)
; -> 3

;; 9.4.1 The floor division operator

;; This might not be what you were expecting!
;; Floating-point numbers keep only the 15 or 16 most important digits (ie the digits at the
;; left of the number, with the highest place values).
;; The philosophy of a ﬂoating-point number is that's close enough, rather than that's the
;; exact value.
;; Suppose you try to deﬁne a symbol PI to store the value of pi to 50 decimal places:

(constant 'PI 3.14159265358979323846264338327950288419716939937510)
;-> 3.141592654

(println PI)


;; It looks like newLISP has cut about 40 digits oﬀ the right hand side! In fact about 15 or
;; 16 digits have been stored, and 35 of the less important digits have been discarded.
;; How does newLISP store this number? Let's look using the format function:


(format "~a" PI)
; -> 3.141592654

(format {%1.50f} PI)
;-> "3.14159265358979311599796346854418516159057617187500"

;; Now let's make a little script to compare both numbers as strings, so we don't have to grep
;; visually the diﬀerences:

(setq original-pi-str "3.14159265358979323846264338327950288419716939937510")
(setq pi (float original-pi-str))
(setq saved-pi-str (format {%1.50f} pi))
(println pi " -> saved pi (float)")
(println saved-pi-str " -> saved pi formatted")
(println original-pi-str " -> original pi")
(dotimes (i (length original-pi-str) (!= (original-pi-str i) (saved-pi-str i)))
(print (original-pi-str i)))
(println " -> original and saved versions are equal up to this")


;; The output will be:
;; 3.141592654 -> saved pi (float)
;; 3.1415926535897931
;; -> saved pi formatted
;; 3.1415926535897932
;; -> original and saved versions are equal up to this

;; 9.4.2 The ceiling and floor functions

;; Notice how the value is accurate up to 9793, but then drifts away from the more precise
;; string you originally supplied. The numbers after 9793 are typical of the way all computers
;; store ﬂoating-point values - it isn't newLISP being creative with your data!
;; The largest ﬂoat you can use seems to be - on my machine, at least - about 10308 . Only the
;; ﬁrst 15 or so digits are stored, though, so that's mostly zeroes, and you can't really add 1
;; to it.
;; Another example of the motto of a ﬂoating-point number: that's close enough!
;; The above comments are true for most computer languages, by the way, not just newLISP.
;; Floating-point numbers are a compromise between convenience, speed, and accuracy.


;; 9.5 The power operator
;; The power operator (^) is used to calculate the result of raising the first number to the
;; power of the second number:

(expt 2 10)
; -> 1024

;; 9.6 The square root operator

;; 9.2 Integer and ﬂoating-point maths

;; When you're working with ﬂoating-point numbers, use the ﬂoating-point arithmetic oper-
;; ators add, sub, mul, div, and mod, rather than +, -, *, /, and %, their integer-only
;; equivalents:

(mul PI 2)
;-> 6.283185307

;; and, to see the value that newLISP is storing (because the interpreter's default output
;; resolution is 9 or 10 digits):

(format {%1.16f} (mul PI 2))
;-> "6.2831853071795862"

;; If you forget to use mul here, and use * instead, the numbers after the decimal point are
;; thrown away:


(format "%.16f" (* PI 2))
;-> "6.283185307179586

;; 9.7 The absolute value operator

(format {%1.16f} (* PI 2))
;-> "6.0000000000000000"


;; 9.8 The sign operator

;; Here, pi was converted to 3 and then multiplied by 2.
;; You can re-deﬁne the familiar arithmetic operators so that they default to using ﬂoating-
;; point routines rather than integer-only arithmetic:
; before

(+ 1.1 1.1)
;-> 2
(constant (global '+) add)
; after
(+ 1.1 1.1)
;-> 2.2

;; 9.9 The comparison operators
;; The comparison operators in newLISP are:
;;  - =
;;  - <
;;  - >
;;  - <=
;;  - >=
;;  - <>
;;  - eq
;;  - ne
;;  - lt
;;  - le
;;  - gt
;;  - ge
;;  - and
;;  - or
;;  - not

;; 9.9.1 eq

;; eq checks if two numbers are equal:

(eq 1 1)
;-> t
(eq 1 2)
;-> nil

;; 9.9.2 eq

;; eq checks if two numbers are equal:

(eq 1 1)
;-> t
(eq 1 2)
;-> nil

;; 9.9.3 eq

;; eq checks if two numbers are equal:

(eq 1 1)

;; You could put these deﬁnitions in your init.lsp ﬁle to have them available for all newLISP
;; work on your machine. The main problem you'll ﬁnd is when sharing code with others, or
;; using imported libraries. Their code might produce surprising results, or yours might!


;; 9.10 The t and nil values

;; t is the symbol for true, and nil is the symbol for false:

(t)
;-> t
(nil)
;-> nil

;; 9.11 The if and cond functions
;; The if function is used to choose between two expressions, depending on a condition:


;; The if function is used to choose between two expressions, depending on a condition:

(if (eq 1 1) "one" "not one")
;-> "one"

;; The cond function is used to choose between a series of expressions, depending on a series
;; of conditions. It's a bit more complicated, but it's a powerful tool for making decisions
;; in your programs:

(cond ((eq 1 1) "one")
       ((eq 1 2) "two")
       (t "unknown"))

;-> "one"

;; 9.12 The do function
;; The do function is used to apply a function to a range of values:

(do ((i 1 (+ i 1)))
       ((> i 5))
       (print i))

;-> 1
;-> 2
;-> 3
;-> 4
;-> 5

;; 9.13 The loop function
;; The loop function is a bit like the do function, but it's more powerful and flexible.
;; The loop function is used to apply a function to a range of values, but it also allows
;; you to specify a starting value, an ending value, and a step size:

(loop for i from 1 to 5 do
       (print i))



;; 9.14 The and and or functions
;; The and function is used to combine multiple conditions into one:

(and (eq 1 1) (eq 2 2))
;-> t

;; The or function is used to combine multiple conditions into one:

(or (eq 1 1) (eq 2 2))
;-> t

;; 9.15 The not function


;; 9.16 The let function
;; The let function is used to create local variables and to bind their values to expressions:

(let ((x 1) (y 2))
       (+ x y))


;; 9.3 Conversions: explicit and implicit

;; To convert strings into numbers, or numbers of one type into another, use the int and ﬂoat
;; functions.
;; The main use for these is to convert a string into a number - either an integer or a ﬂoat.
;; For example, you might be using a regular expression to extract a string of digits from a
;; longer string:

(map int (find-all {\d+} {the answer is 42, not 41}))
;-> (42 41)
; a list of integers

(map float (find-all {\d+(\.\d+)?} {the value of pi is 3.14, not 1.618}))
;-> (3.14 1.618)

;; A second argument passed to int speciﬁes a default value which should be used if the
;; conversion fails:

(int "x")
;-> nil
(int "x" 0)
;-> 0





