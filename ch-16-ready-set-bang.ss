; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 15) The Difference Between Men and Boys...)
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

(define ingredients '())
;
(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))
;
(sweet-toothR 'chocolate) ; (chocolate cake)
ingredients ; (chocolate)
(sweet-toothR 'fruit) ; (fruit cake)
ingredients ; (fruit chocolate)
(sweet-toothR 'cheese) ; (cheese cake)
ingredients ; (cheese fruit chocolate)
;
; ingredients remembers the list of arguments passed to 
; sweet-toothR.
;

; Define deep
;
; Returns 'pizza in a nested list of depth m.
;
(define deep
  (lambda (m)
    (if (zero? m)
        'pizza
         (cons (deep (- m 1)) '()))))
;
(deep 0) ; pizza
(deep 3) ; (((pizza)))

; Define deepR
;
; Same as deep, but remembers all the numbers it has seen
; and the corresponding results.
;
(define Ns '())
(define Rs '())
;
(define deepR
  (lambda (n)
    (let ((result (deep n)))
         (set! Ns (cons n Ns))
         (set! Rs (cons result Rs))
         result)))
;
(deepR 3) ; (((pizza)))
Ns ; (3)
Rs ; ((((pizza))))
(deepR 5) ; (((((pizza)))))
Ns ; (5 3)
Rs ; ((((((pizza))))) (((pizza))))

;
; ###############################################
;
;            The Ninetheenth Commandment
;
;   Use (set! ...) to remember valuable things
;   between two distinct uses of a function.
;
; ###############################################
;

; Define find_
;
; n is guaranteed to occur in Ns. find_ returns the value in 
; the corresponding position of Rs.
;
(define find_
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((= (car ns) n) (car rs))
              (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))
;
(find_ '3 Ns Rs) ; (((pizza)))

; Define member?
;
(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                     (cond
                       ((null? l) #f)
                       (else (or (eq? a (car l)) (yes? (cdr l))))))))
            (yes? lat))))

; Define deepM
;
; Like deepR, but uses find_ to avoid unnecessary consing
; onto Ns.
;
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find_ n Ns Rs)
        (deepR n))))

; Define deepM
;
; Directly, without using deepR
;
(define deepM
  (lambda (n)
    (if (member? n Ns)
        (find_ n Ns Rs)
        (let ((result (deep n)))
             (set! Ns (cons n Ns))
             (set! Rs (cons result Rs))
             result))))
;
(deepM 6) ; ((((((pizza))))))
Ns ; (6 5 3)
Rs ; (((((((pizza)))))) (((((pizza))))) (((pizza))))

; Define deep (improved)
;
; Uses deepM to avoid redundant computations.
;
(define deep
  (lambda (n)
    (cond
      ((zero? n) 'pizza)
      (else (cons (deepM (- n 1)) '())))))
;
(deepM 9) ; (((((((((pizza)))))))))
Ns ; (9 8 7 6 5 3)

; Define deepM (Using sixteenth commandment)
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (if (member? n Ns)
             (find_ n Ns Rs)
             (let ((result (deep n)))
                  (set! Ns (cons n Ns))
                  (set! Rs (cons result Rs))
                  result)))))

; Define find_ (improved)
;
; Returns #f (an atom) if n is not found in Ns.
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

; Define deepM (using the new find_)
;
(define deepM
  (let ((Ns '())
        (Rs '()))
       (lambda (n)
         (let ((found (find_ n Ns Rs)))
              (if (atom? found)
                  (let ((result (deep n)))
                       (set! Ns (cons n Ns))
                       (set! Rs (cons result Rs))
                       result)
                  found)))))

; Define len (using set!)
;
(define len
  (let ((h (lambda (l) 0)))
       (set! h
             (lambda (l)
               (cond
                 ((null? l) 0)
                 (else (+ 1 (h (cdr l)))))))
       h))

;
; ###############################################
;
;            The Seventeenth Commandment
;
;                  (final version)
;
;   Use (set! x ...) for (let ((x ...)) ...)
;   only if there is at least one (lambda ...
;   between it and the (let ...), or if the
;   new value for x is a function that refers
;   to x.
;
; ###############################################
;

; Define L
;
(define L
  (lambda (len)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (+ 1 (len (cdr l))))))))

; Define len (using L)
;
(define len
  (let ((h (lambda (l) 0)))
       (set! h
             (L (lambda (arg) (h arg))))
       h))

; Define Y!
;
; This is the same as len (using L), written as a function of L.
;
(define Y!
  (lambda (L)
    (let ((h (lambda (l) '())))
         (set! h
               (L (lambda (arg) (h arg))))
         h)))

; Define Y-bang
;
; A (letrec ...) is an abbreviation for an expression consisting
; of (let ...) and (set! ...). So another way of writing Y! is 
; Y-bang.
;
(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))
;
; A (letrec ...) that defines mutually recursive definitions can be
; abbreviated using (let ...) and (set! ...) expressions:
;
; (letrec
;   ((x1 a1)
;    ...
;    (xn an))
;    b)
; =
; (let ((x1 0) ... (xn 0))
;      (let ((y1 a1) ... (yn an))
;           (set! x1 y1)
;           ...
;           (set! xn yn))
;      b)
;
; The names y1 ... yn must not occur in a1 ... an and they must
; not be chosen from the names x1 ... xn. Initializing with 0 is
; arbitrary and it is wrong to assume the names x1 ... xn are 0
; in a1 ... an.
;

; Define len (using Y! and L)
;
(define len (Y! L))

; 
; Note:
;   Y-bang is called the applicative-order, imperative Y combinator.
;   It produces recursive definitions without requiring that the
;   functions be named by (define ...).
;

; Define depth* (using Y! and D)
;
(define depth* (Y! D))
;
; Define D
;
(define D
  (define (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s)) (depth* (cdr s)))
        (else
          (max (+ 1 (depth* (car s))) (depth* (cdr s))))))))

;
; Note:
;   (Y! f) builds the corresponding recursive function without
;   (define ...), where f is like the recursive function, except
;   that the name of the recursive function is replaced by the name
;   recfun and the whole expression is wrapped in 
;   (lambda (recfun) ...).
;
;   The function Y! produces the same recursive function as Y for
;   all f that have this shape.
;

; Define Y (from The Little Schemer)
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; Define biz
;
; biz doesn't follow the format of f. It has a let block between 
; define and (lambda (f) ...).
;
(define biz
  (let ((x 0))
       (lambda (f)
         (set! x (+ 1 x))
         (lambda (a)
           (if (= a x) 0 (f a))))))
;
((Y biz) 5) ; 0
;
; ((Y-bang biz) 5) ; No answer
;
; Inside the letrec block of Y-bang,
; (h (f (lambda (arg) (h arg)))), where f is biz,
; gets evaluated to
; (h ((set! x (+ 1 x))
;     (lambda (a)
;       (if (= a x)
;           0
;           ((lambda (arg) (h arg)) a)))))
;   
; Note: (exp1 exp2) evaluates exp1 first and then returns the result
;       of exp2.
;   
; Hence, (set! x (+ 1 x)) is evaluated only once, incrementing x from 
; 0 to 1, and the (lambda (a) ...) part is assigned to h. Now, when h
; is recursively called, x is never incremented. This leads to an 
; infinite loop.
;
