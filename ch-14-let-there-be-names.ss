; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 14) Let There Be Names)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define atom?
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Define letcc
;
(define-syntax letcc
  (syntax-rules ()
    ((letcc var body ...)
     (call-with-current-continuation
       (lambda (var)  body ... )))))


; Define leftmost (from The Little Schemer)
;
; Extracts the leftmost atom from a non-empty list of S-expressions
; that does not contain the empty list.
;
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))
;
(leftmost '(((a) b) (c d)))    ; a
(leftmost '(((a) ()) () (e)))  ; a
;
; (leftmost '(((() a) ())))
; leftmost does not produce any answer for the above as the leftmost
; inner list is empty.
;
; Define leftmost (improved)
;
; This version of leftmost works even when the list of S-exressions 
; contains non-empty lists.
;
(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      ((atom? (leftmost (car l))) (leftmost (car l)))
      (else (leftmost (cdr l))))))
;
(leftmost '(((a) b) (c d)))    ; a
(leftmost '(((a) ()) () (e)))  ; a
(leftmost '(((() a) ())))      ; a
;
; But in the above definition, (leftmost (car l)) is computed twice.
; This can be avoided using let.
;
; Define leftmost (using let)
;
(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (let ((a (leftmost (car l))))
             (cond
               ((atom? a) a)
               (else (leftmost (cdr l)))))))))
;
; Note:
;
; Like (and ...), (let ...) is an abbreviation:
;   (let ((x1 a1) (x2 a2) ... (xn an)) b)
;   = ((lambda (x1 x2 ... xn) b) a1 a2 ... an)
;

; Define rember1*
;
; Removes the first occurrence of a in a list of S-expressions.
;
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
        (cond
          ((eq? (car l) a) (cdr l))
          (else (cons (car l) (rember1* a (cdr l))))))
      (else
        (cond
          ((equal? (rember1* a (car l)) (car l)) (cons (car l) (rember1* a (cdr l))))
          (else (cons (rember1* a (car l)) (cdr l))))))))
;
(rember1* 'salad
          '((Swedish rye)
            (French (mustard salad turkey))
            salad))          ; ((swedish rye) (french (mustard turkey)) salad)
(rember1* 'meat
          '((pasta meat)
            pasta
            (noodles meat sauce)
            meat tomatoes))  ; ((pasta) pasta (noodles meat sauce) meat tomatoes)
;
; Define rember1* (using twelfth commandment)
;
(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
                (cond
                  ((eq? (car l) a) (cdr l))
                  (else (cons (car l) (R (cdr l))))))
              (else
                (cond
                  ((equal? (R (car l)) (car l)) (cons (car l) (R (cdr l))))
                  (else (cons (R (car l)) (cdr l)))))))))
      (R l))))
;
; The above definition computes (R (car l)) twice when a is found in (car l) - once
; during the equal? part (which would be false if a is found in (car l)), and again
; in the else part. The redundant computation can be removed using let.
;
; Define rember1* (using let)
;
(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
                (cond
                  ((eq? (car l) a) (cdr l))
                  (else (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                     (cond
                       ((equal? av (car l)) (cons (car l) (R (cdr l))))
                       (else (cons av (cdr l))))))))))
      (R l))))

; 
; ###############################################
;
;             The Fifteenth Commandment
;             
;               (preliminary version)
;
;   Use (let ...) to name the values of repeated
;   expressions.
;
; ###############################################
;

; Define depth*
;
; Determines the depth of a list of S-expressions (visualize the list
; as a tree, with nested list as sub-trees).
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (cond
          ((> (depth* (cdr l)) (+ 1 (depth* (car l)))) (depth* (cdr l)))
          (else (+ 1 (depth* (car l)))))))))
;
(depth* '((pickled) peppers (peppers pickled)))                    ; 2
(depth* '(c (b (a b) a) a))                                        ; 3
(depth* '(() ((bitter butter) (makes) (batter (bitter))) butter))  ; 4
;
; Define depth* (using let to avoid redundant computations)
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (+ 1 (depth* (car l))))
              (d (depth* (cdr l))))
             (cond
               ((> a d) a)
               (else d)))))))
;
; Define depth* (another version, though no computational advantage)
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
        (let ((d (depth* (cdr l))))
             (cond
               ((atom? (car l)) d)
               (else
                 (let ((a (+ 1 (depth* (car l)))))
                      (cond
                        ((> a d) a)
                        (else d))))))))))

; 
; ###############################################
;
;             The Fifteenth Commandment
;
;                 (revised version)
;
;   Use (let ...) to name the values of repeated
;   expressions in a function definition if they
;   may be evaluated twice for one and the same
;   use of the function.
;
; ###############################################
;

; Define depth* (using if)
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (+ 1 (depth* (car l))))
              (d (depth* (cdr l))))
             (if (> a d) a d))))))
; 
; Define depth* (using max)
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (let ((a (+ 1 (depth* (car l))))
              (d (depth* (cdr l))))
             (max a d))))))
;
; But by using max, there is no redundancy in computation. 
; So, let is not required.
;
; Define depth* (simpler)
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else (max (+ 1 (depth* (car l))) (depth* (cdr l)))))))

; Define pick (from chapter 04 of The Little Schemer)
;
(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (- n 1) (cdr lat))))))

; Define scramble (from chapter 12, using let)
;
(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) '())
              (else
                (let ((rp (cons (car tup) rp)))
                     (cons (pick (car tup) rp) (P (cdr tup) rp))))))))
      (P tup '()))))

; Define leftmost (improved)
;
; Once the leftmost atom is found, it is returned immediately using letcc, 
; without returning it through the recursive stack.
;
(define leftmost
  (lambda (l)
    (letcc skip
      (lm l skip))))
;
(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else
        (let ()
             (lm (car l) out)
             (lm (cdr l) out))))))

;
; Here, (let ...) contains two expressions in the value part.
; When a (let ...) has two expressions in its value part, we must
; first determine the value of the first expression. If it has one, we 
; ignore it and determine the value of the second expression. This is
; also true of (letrec ...) and (letcc ...).
;
; Define leftmost (hiding lm using letrec)
;
(define leftmost
  (letrec
    ((lm (lambda (l out)
           (cond
             ((null? l) '())
             ((atom? (car l)) (out (car l)))
             (else
               (let ()
                    (lm (car l) out)
                    (lm (cdr l) out)))))))
    (lambda (l)
      (letcc skip
        (lm l skip)))))
; 
; Define leftmost (another version)
;
(define leftmost
  (lambda (l)
    (letrec
      ((lm (lambda (l out)
             (cond
               ((null? l) '())
               ((atom? (car l)) (out (car l)))
               (else
                 (let ()
                      (lm (car l) out)
                      (lm (cdr l) out)))))))
      (letcc skip
        (lm l skip)))))
;
; Since skip doesn't change, letrec block can be defined inside
; letcc and twelfth commandment can be used.
;
; Define leftmost (using twelfth commandment)
;
(define leftmost
  (lambda (l)
    (letcc skip
      (letrec
        ((lm (lambda (l)
               (cond
                 ((null? l) '())
                 ((atom? (car l)) (skip (car l)))
                 (else
                   (let ()
                        (lm (car l))
                        (lm (cdr l))))))))
        (lm l)))))

; Define rember1* (improved)
;
; Every time an empty list is encountered, rember1* would not have found
; a in l. Using letcc, the remaining cons and equal? operations can be
; skipped, and we could hop directly to (cdr l).
;
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
        (if (eq? (car l) a)
            (cdr l)
            (cons (car l) (rm a (cdr l) oh))))
      (else
        (if (atom? (letcc oh (rm a (car l) oh)))
            (cons (car l) (rm a (cdr l) oh))
            (cons (rm a (car l) 0) (cdr l)))))))
;
; When a is not found in the list (i.e., when '() is encountered), 'no is
; returned to letcc. But if a had been present in the list, the new list
; (one with the first occurrence of a removed) will be returned. So, if the
; returned value is an atom, then the computation continues on the cdr of the
; list. Otherwise, if it is a list, it is simply cons'ed with (cdr l). 
; 0 is passed as a dummy argument to oh in the last line, as we are sure that
; a is present in the list, and hence '() will not be reached.
;
(define rember1*
  (lambda (a l)
    (if (atom? (letcc oh (rm a l oh)))
        l
        (rm a l '()))))
;
; Here, if the result for the entire list is an atom ('no), then the result
; is the original list (a is not present in l). Otherwise, '() is passed
; as a dummy argument to oh as a is present in l.
;

; Define rember1* (using let)
;
(define rember1*
  (lambda (a l)
    (let ((new-l (letcc oh (rm a l oh))))
         (if (atom? new-l) l new-l))))
;
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
        (if (eq? (car l) a)
            (cdr l)
            (cons (car l) (rm a (cdr l) oh))))
      (else
        (let ((new-car (letcc oh (rm a (car l) oh))))
             (if (atom? new-car)
                 (cons (car l) (rm a (cdr l) oh))
                 (cons new-car (cdr l))))))))

; Define try
;
; try is not available in r5rs scheme. It can be implemented using macros.
; Like (and ...), (try ...) is an abbreviation:
; (try x a b) 
; = (letcc success
;     (letcc x (success a))
;     b)
; The name success must not occur in a or b.
;
(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (letcc success 
       (letcc var (success a)) . b))))

; Define rember1* (simplified using try)
;
; This version doesn't depend on oh being an atom.
;
(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))
;
(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
        (if (eq? (car l) a)
            (cdr l)
            (cons (car l) (rm a (cdr l) oh))))
      (else
        (try oh2
          (cons (rm a (car l) oh2) (cdr l))
          (cons (car l) (rm a (cdr l) oh)))))))
