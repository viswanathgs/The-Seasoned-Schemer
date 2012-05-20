; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 11) Welcome Back to the Show)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define atom?
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Define two-in-a-row?
; 
; Determines whether any atom occurs twice in a row
; in a list of atoms.
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
        (or (is-first? (car lat) (cdr lat)) (two-in-a-row? (cdr lat)))))))
;
; Define is-first?
;
; Checks if the first element of lat is eq? to a.
;
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? a (car lat))))))
;
(two-in-a-row? '(Italian sardines spaghetti parsley))          ; #f
(two-in-a-row? '(Italian sardines sardines spaghetti parsley)) ; #t
(two-in-a-row? '(Italian sardines more sardines spaghetti))    ; #f

; Define two-in-a-row? (revised)
; 
; This version leaves the decision to resume search in the rest of lat
; to is-first?
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (is-first-b? (car lat) (cdr lat))))))
;
; Define is-first-b?
;
(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else
        (or (eq? a (car lat)) (two-in-a-row? lat))))))

; Define two-in-a-row-b? (revised)
;
; When two-in-a-row? is called in the is-first-b? function, lat is
; never empty. So, we could write a function like is-first-b? that 
; doesn't use two-in-a-row? at all.
;
; preceding is the atom that precedes the atoms in lat in the 
; original list.
;
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else
        (or (eq? preceding (car lat)) (two-in-a-row-b? (car lat) (cdr lat)))))))
;
; Define two-in-a-row? (using two-in-a-row-b?)
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

; Define sum-of-prefixes
;
; Takes a tup, and returns a tup of equal length, where each value is the sum of 
; all the prefixes for each element in the original tup.
;
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))
;
; Define sum-of-prefixes-b
;
; sonssf is the sum of numbers seen so far
;
(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else 
        (cons (+ sonssf (car tup)) 
              (sum-of-prefixes-b (+ sonssf (car tup)) (cdr tup)))))))
;
(sum-of-prefixes '(2 1 9 17 0)) ; (2 3 12 29 29)
(sum-of-prefixes '(1 1 1 1 1))  ; (1 2 3 4 5)

;
; ###############################################
;
;              The Eleventh Commandment
;
;    Use additional arguments when a function
;    needs to know what other arguments to the
;    function have been like so far.
;
; ###############################################
;

; Define scramble
;
; Takes a non-empty tup in which no number is greater than
; its own index (1-based) and returns a tup of the same length.
; Each number in the argument is treated as a backward index
; from its own position to a point earlier in the tup. The result
; at each position is found by counting backward from the current
; position according to this index.
;
(define scramble
  (lambda (tup)
    (scramble-b tup '())))
;
; Define scramble-b
;
; rev-pre is the reversed (proper) prefix to tup.
;
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
        (cons (pick (car tup) (cons (car tup) rev-pre))
              (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))
;
; Define pick (from chapter 04 of The Little Schemer)
;
(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (- n 1) (cdr lat))))))
;
(scramble '(1 1 1 3 4 2 1 1 9 2))    ; (1 1 1 1 1 4 1 1 1 9)
(scramble '(1 2 3 4 5 6 7 8 9))      ; (1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 1 2 3 4 1 8 2 10)) ; (1 1 1 1 1 1 1 1 2 8 2)
