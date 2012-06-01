; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 17) We Change, Therefore We Are!)
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

; Define try
;
(define-syntax try 
  (syntax-rules () 
    ((try var a . b) 
     (letcc success 
       (letcc var (success a)) . b))))

; Define deep (from chapter 16)
;
(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (cons (deep (- n 1) '())))))

; Define find_ (from chapter 16)
;
(define find_
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= (car ns) n) (car rs))
              (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

; Define deepM (from chapter 16)
;
; Here, deep is included inside letrec.
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (letrec
         ((D (lambda (m)
               (if (zero? m) 'pizza (cons (D (- m 1)) '())))))
         (lambda (n)
           (let ((found (find_ n Ns Rs)))
                (if (atom? found)
                    (let ((result (D n)))
                         (set! Ns (cons n Ns))
                         (set! Rs (cons result Rs))
                         result)
                    found))))))

; Define deepM (improved)
;
; D can recursively call deepM instead of D to make use of the memoized
; values in Ns and Rs. Since D would not call itself, it can be included
; inside let instead of letrec. Also, since D does not refer to Ns or
; Rs, D can be defined in the same let block as Ns and Rs.
;
(define deepM
  (let ((Ns '())
        (Rs '())
        (D (lambda (m)
             (if (zero? m) 'pizza (cons (deepM (- m 1)) '())))))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result (D n)))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))
;
; The usage of D can be replaced by the definition itself.
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result ((lambda (m)
                                   (if (zero? m) 'pizza (cons (deepM (- m 1) '())))) 
                                 n)))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))
;
; Applying (lambda ...) immediately to an argument is equivalent to (let ...).
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result (let ((m n))
                                     (if (zero? m) 'pizza (cons (deepM (- m 1)) '())))))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))
;
; The (let ...) seems unnecessary.
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result (if (zero? n) 'pizza (cons (deepM (- n 1)) '()))))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))

; Define consC
;
; Same as cons, but counts how many times it sees arguments.
;
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (+ N 1))
      (cons x y))))

; Define deep (using consC)
;
; Direct version, without memoization using deepM.
;
(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (consC (deep (- n 1)) '()))))

; Define consC (improved)
;
; The N inside the let block of consC cannot be accessed from outside. 
; Define a function to return the value of N.
;
(define counter)
;
; The above incomplete definition is ok as we don't care for the initial
; value and we change it immediately inside consC using set!.
;
(define consC
  (let ((N 0))
       (set! counter (lambda () N))
       (lambda (x y)
         (set! N (+ N 1))
         (cons x y))))
;
(deep 5) ; (((((pizza)))))
(counter) ; 5
(deep 3) ; (((pizza)))
(counter) ; 8

; Define supercounter
;
; Applies the function f for all arguement values from 0 to 1000. Returns
; the answer of (counter).
;
(define supercounter
  (lambda (f)
    (letrec
      ((S (lambda (n)
            (if (zero? n)
                (f n)
                (let ()
                  (f n)
                  (S (- n 1)))))))
      (S 1000)
      (counter))))
;
(supercounter deep) ; 500508
;
; The answer should be 500500 (0 + 1 + 2 + ... + 1000). The extra 8 are the
; leftovers from previous experiments.
;

; Define consC (improved)
;
; Include set-counter to reset the value of N.
;
(define counter)
(define set-counter)
;
(define consC
  (let ((N 0))
    (set! counter (lambda () N))
    (set! set-counter (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (+ N 1))
      (cons x y))))
;
(supercounter deep) ; 500500
(set-counter 0)
(counter) ; 0

; Define deepM (using consC)
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result (if (zero? n) 'pizza (consC (deepM (- n 1)) '()))))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))
;
(set-counter 0)
(counter) ; 0
(deepM 5) ; (((((pizza)))))
(counter) ; 5
(deepM 7) ; (((((((pizza)))))))
(counter) ; 7
(deepM 3) ; (((pizza)))
(counter) ; 7
(supercounter deepM) ; 1000

; Define rember1*C
;
; Like rember1*C (the final version using letcc) from chapter 14, but with consC.
;
(define rember1*C
  (lambda (a l)
    (letrec
      ((R (lambda (l oh)
            (cond
              ((null? l) (oh 'no))
              ((atom? (car l))
                (if (eq? (car l) a) 
                    (cdr l) 
                    (consC (car l) (R (cdr l) oh))))
              (else
                (let ((new-car (letcc oh (R (car l) oh))))
                  (if (atom? new-car)
                      (consC (car l) (R (cdr l) oh))
                      (consC new-car (cdr l)))))))))
      (let ((new-l (letcc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))
;
(set-counter 0)
(counter) ; 0
(rember1*C 'noodles
           '((food) more (food))) ; ((food) more (food))
(counter) ; 0
; 
; The counter value above is 0 because consC was never used. Since a was not found
; in l, we always used letcc to hop without cons'ing.
;

; Define rember1*C2
;
; Another version of rember1*C from chapter 14. This is the version that doesn't use
; letcc, but instead checks for equality of lists to determine whether the atom was
; found and removed. This is rewritten using consC.
;
(define rember1*C2
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
                (if (eq? (car l) a)
                    (cdr l)
                    (consC (car l) (R (cdr l)))))
              (else
                (let ((av (R (car l))))
                  (if (equal? (car l) av)
                      (consC (car l) (R (cdr l)))
                      (consC av (cdr l)))))))))
      (R l))))
;
(set-counter 0)
(rember1*C2 'noodles
            '((food) more (food))) ; ((food) more (food))
(counter) ; 5
;
; Since we don't use letcc to hop here, 5 cons'es are used to rebuild l.
; 
