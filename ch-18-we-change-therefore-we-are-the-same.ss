; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 18) We Change, Therefore We Are the Same!)
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
; Define lots
;
; Returns a list of m 'eggs.
;
(define lots
  (lambda (m)
    (cond
     ((zero? m) '())
     (else (kons 'egg (lots (- m 1)))))))

; Define lenkth
;
; Similar to length.
;
(define lenkth
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (+ 1 (lenkth (kdr l)))))))

; Define add-at-end
;
; Adds 'egg to the end of a non-empty list.
;
(define add-at-end
  (lambda (l)
    (cond
     ((null? (kdr l)) (konsC (kar l) (kons 'egg '())))
     (else (konsC (kar l) (add-at-end (kdr l)))))))

; Define add-at-end-too
;
; Similar to add-at-end, but without making any new konses
; except for the last one.
;
(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
               ((null? (kdr ls)) (set-kdr ls (kons 'egg '())))
               (else (A (kdr ls)))))))
      (A l)
      l)))

; Define kons
;
; Similar to cons
;
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))
;
; Define kar
;
(define kar
  (lambda (c)
    (c (lambda (a d) a))))
;
; Define kdr
;
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))

; Define bons
;
(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))
;
; Define kar
;
(define kar
  (lambda (c)
    (c (lambda (s a d) a))))
;
; Define kdr
;
(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))
;
; Define set-kdr
;
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))
;
; Define kons (using bons and set-kdr)
;
(define kons
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

; Define konsC
;
(define kounter)
(define set-counter)
;
(define konsC
  (let ((N 0))
    (set! kounter (lambda () N))
    (set! set-counter
          (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (+ N 1))
      (kons x y))))

(define dozen (lots 12)) ; 12 konses used
(define bakers-dozen (add-at-end dozen)) ; 13 konses used. 25 konses used altogether.
(define bakers-dozen-too (add-at-end-too dozen)) ; 1 kons used. 26 konses altogether.
;
; dozen is now changed after the previous add-at-end-too. It contains 13 atoms, not 12.
;
(define bakers-dozen-again (add-at-end dozen)) ; 14 konses used. 40 konses altogether.

; Define eklist?
;
(define eklist?
  (lambda (ls1 ls2)
    (cond
     ((null? ls1) (null? ls2))
     ((null? ls2) #f)
     (else
      (and (eq? (kar ls1) (kar ls2))
           (eklist? (kdr ls1) (kdr ls2)))))))
;
(eklist? bakers-dozen bakers-dozen-too) ; #t

; Define same?
;
; Two konses are the same if changing one changes the other.
; This function temporarily changes the kdrs of two konses.
; Then, if changing the second kons also affects the first
; kons, the two must be the same.
;
(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))
;
(same? dozen bakers-dozen) ; #f
(same? dozen bakers-dozen-too) ; #t
(same? (kons 'egg '()) (kons 'egg '())) ; #f

; Define last-kons
;
; Returns the last kons in a non-empty kons list.
;
(define last-kons
  (lambda (ls)
    (cond
     ((null? (kdr ls)) ls)
     (else (last-kons (kdr ls))))))
;
(define long (lots 12))
(lenkth long) ; 12
;
(set-kdr (last-kons long) long)
(lenkth long) ; Aborting!: maximum recursion depth exceeded
;
; This is because the last kons of long no longer contains '() in the
; kdr part. Instead, the kdr part refers to some kons inside of long.
; Hence, lenkth keeps looping forever.
;

; Define finite-lenkth
;
; Returns its argument's length, if it has one. If the argument doesn't have
; a length, returns #f.
;
; Floyd's cycle-finding algorithm (Tortoise and hare algorithm)
;
(define finite-lenkth
  (lambda (p)
    (letcc infinite
           (letrec
               ((C (lambda (p q)
                     (cond
                       ((same? p q) (infinite #f))
                       ((null? q) 0)
                       ((null? (kdr q)) 1)
                       (else
                        (+ 2 (C (slow p) (quick q)))))))
                (slow (lambda (x) (kdr x)))
                (quick (lambda (x) (kdr (kdr x)))))
             (cond
              ((null? p) 0)
              (else (+ 1 (C p (kdr p)))))))))