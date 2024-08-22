;; int is a clever function that can also convert strings representing numbers in number bases
;; other than 10 into numbers. For example, to convert a hexadecimal number in string form
;; to a decimal number, make sure it is preﬁxed with 0x, and don't use letters beyond f:

(int (string "0x" "1F"))
;-> 31
(int (string "0x" "decaff"))
;-> 14600959


;; Here's a more complex function that converts a hexadecimal number to binary using recursion:

(define (hex_to_binary hex_num)
  (cond
    ((equal? hex_num "") "")
    ((equal? hex_num "0") "0")
    ((equal? hex_num "1") "1")
    ((equal? hex_num "2") "10")
    ((equal? hex_num "3") "11")
    ((equal? hex_num "4") "100")
    ((equal? hex_num "5") "101")
    ((equal? hex_num "6") "110")
    ((equal? hex_num "7") "111")
    ((equal? hex_num "8") "1000")
    ((equal? hex_num "9") "1001")
    ((equal? hex_num "A") "1010")
    ((equal? hex_num "B") "1011")
    ((equal? hex_num "C") "1100")
    ((equal? hex_num "D") "1101")
    ((equal? hex_num "E") "1110")
    ((equal? hex_num "F") "1111")
    (else (string (hex_to_binary (substring hex_num 1)) (string
    (char->integer (string-ref hex_num 0)) ; convert the first character to
    ;; its decimal equivalent
    (modulo 2))))))
    ;; the last character of the hex_num string, and recursively call the
    ;; function with the rest of the string
    ;; multiplied by 2 (to shift the bits to the left) and the decimal equivalent of
    ;; the first character modulo 2 (to get the last bit)

    ;; Note: This function assumes that the input string is a valid hexadecimal number.
    ;; If you need to handle invalid inputs, you can add error handling code.

    ;; Example usage:
    ;; (hex_to_binary "1F") ;-> "1111"
    ;; (hex_to_binary "decaff") ;-> "11011
    ;; (hex_to_binary "1234567890ABCDEF")
    ;; ;-> "1111000010110010
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111

    ;; (hex_to_binary "GHIJKLMNOPQRSTUVWXYZ") ;
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111

    ;; (hex_to_binary "abcdefghijklmnopqrstuvwxyz") ;
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000

    ;; (hex_to_binary "1234567890abcdef1
    ;; ;-> "1111000010110010
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000

    ;; (hex_to_binary "GHIJKLMNOPQRSTUVWXYZ12
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000

    ;; (hex_to_binary "abcdefghijklmnopqrstuvwxyz1234567
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000


    ;; (hex_to_binary "abcdefghijklmnopqrstuvwxyz1234567
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000
    ;; ;-> "1111111111111111
    ;; ;-> "1000000000000000

;; And you can convert strings containing octal numbers by preﬁxing them with just a 0:

(int "035")
;-> 29

;; Binary numbers can be converted by preﬁxing them with 0b:
(int "0b100100100101001001000000000000000000000010100100")
;-> 160881958715556

;; Even if you never use octal or hexadecimal, it's worth knowing about these conversions,
;; because one day you might, either deliberately or accidentally, write this:

(int "08")


;; which evaluates to 0 rather than 8 - a failed octal-decimal conversion rather than the decimal
;; 8 that you might have expected! For this reason, it's always a good idea to specify not only
;; a default value but also a number base whenever you use int on string input:

(int "08" 0 10)
;-> 8


;; (hex_to_binary "08")
;; ;-> "1000"

;; (octal_to_binary "08")
;; ;-> "1000"


;; (binary_to_octal "1000")
;; ;-> "10"

;; (binary_to_hex "1000")
;; ;-> "8"

;; (octal_to_hex "10")
;; ;-> "8"

;; (hex_to_octal "8")
;; ;-> "10"

;; (hex_to_binary "8")
;; ;-> "1000"

;; (binary_to_octal "1000")
;; ;-> "10"

;; (octal_to_binary "10")
;; ;-> "1000"


;; (binary_to_hex "1000")
;; ;-> "8"

;; (hex_to_binary "8")
;; ;-> "1000"

;; If you're working with big integers (integers larger than 64-bit integers), use bigint rather
;; than int. See Bigger numbers2 .

;; 9.4 Invisible conversion and rounding

;; Some functions convert ﬂoating-point numbers to integers automatically. Since newLISP
;; version 10.2.0 all operators made of letters of the alphabet produce ﬂoats and operators
;; written with special characters produce integers.
;; So using ++ will convert and round your numbers to integers, and using inc will convert
;; your numbers to ﬂoats:

(setq an-integer 2)
;-> 2

(float? an-integer)
;-> nil

(inc an-integer)
;-> 3

(float? an-integer)
;-> true

(setq a-float (sqrt 2))
;-> 1.414213562

(integer? a-float)
;-> nil

(++ a-float)
;-> 2

(integer? a-float)
;-> true

;; To make inc and dec work on lists you need to access speciﬁc elements or use map to
;; process all:

(setq numbers '(2 6 9 12))
;-> (2 6 9 12)

(inc (numbers 0))
;-> 3

numbers
;-> (3 6 9 12)

(map inc numbers)
;-> (4 7 10 13)
; but WATCH OUT!

(map (curry inc 3) numbers) ; this one doesn't produce what you expected
;-> (6 12 21 33)
; use this instead:

(map (curry + 3) numbers)
;-> (6 9 12 15)


;; Many newLISP functions automatically convert integer arguments into ﬂoating-point values.
;; This usually isn't a problem. But it's possible to lose some precision if you pass extremely
;; large integers to functions that convert to ﬂoating-point:

(format {%15.15f} (add 1 922337203685477580))
;-> "922337203685477632.000000000000000"

;; Because the add function converted the very large integer to a ﬂoat, a small amount of
;; precision was lost (amounting to about 52, in this case). Close enough? If not, think
;; carefully about how you store and manipulate numbers.

;; 10.5 Strings and symbols

;; Strings are sequences of characters. You can create a string using double quotes:

(defvar a-string "Hello, world!")

;; You can access individual characters of a string using the string-ref function:

(string-ref a-string 6)
;-> "!"

;; You can concatenate strings using the string-concatenate function:

(string-concatenate "Hello, " "world!")
;-> "Hello, world!"

;; Symbols are names for values. You can create a symbol using the symbol function:

(defvar a-symbol (symbol "Hello, world!"))

;; You can test if two symbols are equal using the eq function:

(eq a-string a-symbol)
;-> nil

;; You can get the string representation of a symbol using the symbol-name function:

(symbol-name a-symbol)
;-> "Hello, world!"

;; You can get the value of a symbol using the symbol-value function:

(symbol-value a-symbol)
;-> "Hello, world!"

;; You can set the value of a symbol using the setf function:

(setf a-symbol "Goodbye, world!")

;; You can test if a symbol is bound to a value using the boundp function:

(boundp a-symbol)
;-> t

;; You can unbind a symbol using the unbind function:

(unbind a-symbol)




;; 9.5 Number testing
;; Sometimes you will want to test whether a number is an integer or a ﬂoat:

(set 'PI 3.141592653589793)
;-> 3.141592654

(integer? PI)
;-> nil

(float? PI)
;-> true

(number? PI)
;-> true

(zero? PI)
;-> nil

;; With integer? and ﬂoat?, you're testing whether the number is stored as an integer or
;; ﬂoat, not whether the number is mathematically an integer or a ﬂoating-point value. For
;; example, this test returns nil, which might surprise you:

(integer? (div 30 3))
;-> nil


;; It's not that the answer isn't 10 (it is), but rather that the answer is a ﬂoating-point 10,
;; not an integer 10, because the div function always returns a ﬂoating-point value.


;; 9.6 Absolute signs, from ﬂoor to ceiling

;; It's worth knowing that the ﬂoor and ceil functions return ﬂoating-point numbers that
;; contain integer values. For example, if you use ﬂoor to round pi down to the nearest
;; integer, the result is 3, but it's stored as a ﬂoat not as an integer:

(integer? (floor PI))
;-> nil

(floor PI)
;-> 3

(float? (ceil PI))
;-> true


;; The abs and sgn functions can also be used when testing and converting numbers. abs
;; always returns a positive version of its argument, and sgn returns 1, 0, or -1, depending on
;; whether the argument is positive, zero, or negative.
;; The round function rounds numbers to the nearest whole number, with ﬂoats remaining
;; ﬂoats. You can also supply an optional additional value to round the number to a speciﬁc
;; number of digits. Negative numbers round after the decimal point, positive numbers round
;; before the decimal point.


;; 9.7 Numbers in other bases

;; There are several functions available in newLISP to convert numbers between different bases:

;; bin, oct, dec, hex
;; These functions convert numbers from decimal to binary, octal, decimal, and hexadecimal,
;; respectively. For example:

(bin 10)

(set 'n 1234.6789)
     (for (i -6 6)
     (println (format {%4d %12.5f} i (round n i))))



;; The rational-to-string function converts a rational number to a string:

(rational-to-string (make-ratio 3 4))
;-> "3/4"

;; The string-to-rational function converts a string to a rational number:

(string-to-rational "3/4")
;-> (3/4)

;; The string-to-number function converts a string to a number:

(string-to-number "1234.5678")
;-> 1234.5678

;; The number->string function converts a number to a string:

(number->string 1234.5678)
;-> "1234.5678"

;; The number->integer function converts a number to an integer:

(number->integer 1234.5678)
;-> 1234

;; The integer->number function converts an integer to a number:

(integer->number 1234)
;-> 1234.0

;; The number->hex function converts a number to a hexadecimal string:

(number->hex 1234.5678)
;-> "4D2.1D4"

;; The hex->number function converts a hexadecimal string to a number:

(hex->number "4D2.1D4")

;; sgn has an alternative syntax that lets you evaluate up to three diﬀerent expressions de-
;; pending on whether the ﬁrst argument is negative, zero, or positive.

(for (i -5 5)
     (println i " is " (sgn i "below 0" "0" "above 0")))

;; 9.7 Number formatting
;; To convert numbers into strings, use the string and format functions:

(reverse (string PI))
;-> "456395141.3"

;; Both string and println use only the ﬁrst 10 or so digits, even though more (up to 15 or
;; 16) are stored internally.
;; Use format to output numbers with more control:

(format {%1.15f} PI)
;-> "3.141592653589793"


;; 9.8 Number precision
;; newLISP uses double-precision floating-point arithmetic, which gives it about 15
;; decimal digits of precision. This is adequate for most practical purposes, but if you
;; need more, you can use the gensym function to generate unique symbols and use them as
;; temporary variables:

(let ((x (gensym "X")))
      (format (symbol-name x) "%1.15f" x))
      (format (symbol-name x) "%1.15f" (expt 2
              (floor (log 10 (* (expt 2 15) 10
              (abs (expt 2 (floor (log 10 x)))))))))



;; 9.9 Number manipulation
;; Here are some simple examples of how to manipulate numbers in newLISP:

(set 'x 3)
(set 'y 4)

;; Addition:
(add x y)
;-> 7

;; Subtraction:
(subtract x y)
;-> -1

;; Multiplication:
(multiply x y)
;-> 12

;; Division:
(divide x y)
;-> 0.75

;; Exponentiation:
(expt x y)
;-> 81

;; Floor division:
(floor (/ x y))
;-> 0

;; Remainder of floor division:
(mod x y)
;-> 3

;; Greatest common divisor:
(gcd x y)

;; 9.7 Number formatting
;; To convert numbers into strings, use the string and format functions:

(reverse (string PI))
;-> "456395141.3"

;; Both string and println use only the ﬁrst 10 or so digits, even though more (up to 15 or
;; 16) are stored internally.
;; Use format to output numbers with more control:

(format {%1.15f} PI)
;-> "3.141592653589793"

;; The format speciﬁcation string uses the widely-adopted printf-style formatting. Remember
;; too that you can use the results of the format function:

(string "the value of pi is " (format {%1.15f} PI))
;-> "the value of pi is 3.141592653589793"
;; The format function lets you output numbers as hexadecimal strings as well:

(format "%x" 65535)
;-> "ffff"

;; 9.8 Number utilities

;; 9.8.1 Creating numbers
;; There are some useful functions that make creating numbers easy.

;; 9.9 Sequences and series
;; sequence produces a list of numbers in an arithmetical sequence. Supply start and ﬁnish
;; numbers (inclusive), and a step value:

(sequence 1 10 1.5)
;-> (1 2.5 4 5.5 7 8.5 10)

;; If you specify a step value, all the numbers are stored as ﬂoats, even if the results are
;; integers, otherwise they're integers:
; with step value sequence gives floats

(sequence 1 10 2)
;-> (1 3 5 7 9)

(map float? (sequence 1 10 2))
;-> (true true true true true)
; without step value sequence gives integers

(sequence 1 5)
;-> (1 2 3 4 5)


;; 9.9.2 Creating lists
;; list creates a list of elements:

(list 1 2 3 4 5)
;; 9.9.3 Creating vectors
;; vector creates a vector of elements:

(vector 1 2 3 4 5)

;; 9.9.4 Creating strings
;; string creates a string from a list of characters:

;; list creates a list of elements:

(string-join (list 1 2 3 4 5) "")
;-> "12345"

;; 9.9.5 Creating sets
;; set creates a set of elements:

(set 1 2 3 2 1)
;-> (1 2 3)

;; 9.9.6 Creating dictionaries
;; dict creates a dictionary with key-value pairs:


(map float? (sequence 1 5))
;-> (nil nil nil nil nil)

;; series multiplies its ﬁrst argument by its second argument a number of times. The number
;; of repeats is speciﬁed by the third argument. This produces geometric sequences:

(series 1 2 20)
;-> (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288)

;; Every number is stored as a ﬂoat.
;; The second argument of series can also be a function. The function is applied to the ﬁrst
;; number, then to the result, then to that result, and so on.


;; 9.9.7 Creating matrices
;; matrix creates a matrix of elements:

(series 10 sqrt 20)
;-> ((10.0 1.0) (1.0 1.4
(matrix-transpose (matrix-multiply (matrix-transpose (matrix-transpose matrix)) matrix))


;; 9.9.8 Creating tensors
;; tensor creates a tensor of elements:


;; 9.9.9 Creating rational numbers
;; rational creates a rational number from two integers:

(rational 3 4)
;-> (3/4)

;; 9.9.10 Creating complex numbers
;; complex creates a complex number from two real numbers:

(complex 3 4)
;-> (3+4i)

;; 9.9.11 Creating quaternions
;; quaternion creates a quaternion from four real numbers:

(quaternion 3 4 5 6)
;-> (3+4i+5j+6k)

;; 9.9.12 Creating symbolic expressions
;; symbol creates a symbol:

(symbol 'x)
;-> x

;; 9.9.13 Creating matrices of symbolic expressions
;; matrix-of-symbols creates a matrix of symbols:


;; 9.9.14 Creating tensors of symbolic expressions
;; tensor-of-symbols creates a tensor of symbols:

;; 9.9.15 Creating rational numbers of symbolic expressions
;; rational-of-symbols creates a rational number of symbols:

;; 9.9.16 Creating complex numbers of symbolic expressions
;; complex-of-symbols creates a complex number of symbols:

;; 9.9.17 Creating quaternions of symbolic expressions
;; quaternion-of-symbols creates a quaternion of symbols:

;; 9.9.18 Creating symbolic expressions of symbolic expressions
;; symbolic-of-symbols creates a symbolic expression of symbols:

;; The normal function returns a list of ﬂoating-point numbers with a speciﬁed mean and a
;; standard deviation. For example, a list of 6 numbers with a mean of 10 and a standard
;; deviation of 5 can be produced as follows:

(normal 10 5 6)
;-> (6.5234375 14.91210938 6.748046875 3.540039062 4.94140625 7.1484375)


;; 9.9.19 Creating symbolic expressions of symbolic expressions
;; symbolic-of-symbolic-of-symbols creates a symbolic expression of symbolic expressions:

;; 9.9.20 Creating symbolic expressions of symbolic expressions of symbolic expressions
;; symbolic-of-symbolic-of-symbolic-of-symbols creates a symbolic expression
;; of symbolic expressions of symbolic expressions:

;; 9.10 Random numbers
;; rand creates a list of randomly chosen integers less than a number you supply:

(rand 7 20)
; 20 numbers between 0 and 6 (inclusive) or 7 (exclusive)
;-> (0 0 2 6 6 6 2 1 1 1 6 2 0 6 0 5 2 4 4 3)

;; randn creates a list of normally distributed random numbers with a mean and a
;; standard deviation you supply:

(randn 0 1 10)
;-> (0.862851888982301
(randn 0 1 10)
;-> 0.116205659977726
(randn 0 1 10)
;-> 0.476017968722764
;-> ...

;; 9.11 Random number generators
;; rand-gen creates a random number generator with a given seed:

(let ([rand-gen (rand-gen 1000)])
  (map (lambda () (rand-gen rand-gen)) 10))
  ;-> (916 674 477 214 6
  ;-> 749 665 915 547 5


;; 9.12 Random number distributions
;; rand-normal creates a random number generator with a mean and a standard
;; deviation:


;; 9.13 Random number distributions of symbolic expressions
;; rand-normal-of-symbols creates a random number generator with a mean and a
;; standard deviation of symbolic expressions:

;; 9.14 Random number distributions of symbolic expressions of symbolic expressions
;; rand-normal-of-symbolic-of-symbols creates a random number generator with a
;; mean and a standard deviation of symbolic expressions of symbolic expressions:


;; 9.10 Random numbers
;; rand creates a list of randomly chosen integers less than a number you supply:

(rand 7 20)
; 20 numbers between 0 and 6 (inclusive) or 7 (exclusive)
;-> (0 0 2 6 6 6 2 1 1 1 6 2 0 6 0 5 2 4 4 3)

;; Obviously (rand 1) generates a list of zeroes and isn't useful. (rand 0) doesn't do anything
;; useful either, but it's been assigned the job of initializing the random number generator.
;; If you leave out the second number, it just generates a single random number in the range.
;; random generates a list of ﬂoating-point numbers multiplied by a scale factor, starting at
;; the ﬁrst argument:

(random 0 2 10)


;; 9.11 Random number generators
;; rand-gen creates a random number generator with a given seed:

(let ([rand-gen (rand-gen 1000)])
  (map (lambda () (rand-gen rand-gen)) 10))
  ;-> (916 674 477 214 6

;; 9.11 Randomness

;; Use seed to control the randomness of rand (integers), random (ﬂoats), randomize
;; (shuﬄed lists), and amb (list elements chosen at random).
;; If you don't use seed, the same set of random numbers appears each time. This provides
;; you with a predictable randomness - useful for debugging. When you want to simulate the
;; randomness of the real world, seed the random number generator with a diﬀerent value each
;; time you run the script:
;; Without seed:
; today
(for (i 10 20)
     (print (rand i) { }))

     ; With seed:
     (let ([seed (current-seconds)])
       (rand-gen seed)
       (for (i 10 20)
        (print (rand i) { }))))
        ;-> 0.792124325853712
        ;-> 0.162521513452429
        ;-> 0.491623551609599

        ; Without seed:
        (for (i 10 20)
         (print (rand i) { }))

         ; With seed:
         (let ([seed (current-seconds)])
           (rand-gen seed)
           (for (i 10 20)
            (print (rand i) { }))))
            ;-> 0.792124325853712
            ;-> 0.162521513452429
            ;-> 0.491623551609599
            ;-> ...

;; 9.12 Random number distributions
;; rand-normal creates a random number generator with a mean and a standard
;; deviation:

(let ([rand-gen (rand-normal 0 1 10)])
  (map (lambda () (rand-gen)) 10))
  ;-> (0.862851888982301
  ;-> 0.116205659977726
  ;-> 0.476017968722764
  ;-> ...

; tomorrow
(for (i 10 20)
     (print (rand i) { }))  

      ; With seed:
      (let ([seed (current-seconds)])
       (rand-gen seed)
       (for (i 10 20)
        (print (rand i) { }))))
        ;-> 0.696389645371238
        ;-> 0.095638771446841
        ;-> 0.768466554924277
        ;-> ...
        ; With seed:
        (let ([seed (current-seconds)])
         (rand-gen seed)
         (for (i 10 20)
          (print (rand i) { }))))
          ;-> 0.696389645371238
          ;-> 0.095638771446841
          ;-> 0.768466554924277
          ;-> ...


;; 9.13 Random number distributions of symbolic expressions
;; rand-normal-of-symbols creates a random number generator with a mean and a
;; standard deviation of symbolic expressions:

;; 9.14 Random number distributions of symbolic expressions of symbolic expressions
;; rand-normal-of-symbolic-of-symbols creates a random number generator with a
;; mean and a standard deviation of symbolic expressions of symbolic expressions:


;; 9.12 Random number distributions
;; rand-normal creates a random number generator with a mean and a standard
;; deviation:

(let ([rand-gen (rand-normal 0 1 10)])
  (map (lambda () (rand-gen)) 10))
  ;-> (0.862851888982301
  ;-> 0.116205659977726
  ;-> 0.476017968722764
  ;-> ...
  ; tomorrow
  (for (i 10 20)
   (print (rand i) { }))  

      ; With seed:
      (let ([seed (current-seconds)])
       (rand-gen seed)
       (for (i 10 20)
        (print (rand i) { }))))
        ;-> 0.696389645371238

