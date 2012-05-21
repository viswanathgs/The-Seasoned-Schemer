; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 13) Hop, Skip, and Jump)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define atom?
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Define member?
;
(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                     (cond
                       ((null? l) #f)
                       (else (or (eq? a (car l)) (yes? (cdr l))))))))
            (yes? lat))))

; Define intersect (from The Little Schemer)
;
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))
;
; Define intersect (using twelfth commandment)
;
(define intersect
  (lambda (set1 set2)
    (letrec 
      ((I (lambda (set)
            (cond
              ((null? set) '())
              ((member? (car set) set2) (cons (car set) (I (cdr set))))
              (else (I (cdr set)))))))
      (I set1))))
;
(intersect '(tomatoes and macaroni) '(macaroni and cheese)) ; (and macaroni)

; Define intersectall (from The Little Schemer)
;
; Intersects a list of sets.
;
(define intersectall
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset) (intersectall (cdr lset)))))))
;
; The first question in cond above is to make sure that lset
; is not empty before the function goes through the list of
; sets. Once we know it isn't empty, we never have to ask
; the question again.
;
; Define intersectall (using letrec)
;
; In this version, (null? lset) is asked only once (for the original
; list).
;
(define intersectall
  (lambda (lset)
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
              (else (intersect (car lset) (A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))
;
(intersectall '((3 mangoes and)
                (3 kiwis and)
                (3 hamburgers)))       ; (3)
(intersectall '((3 steaks and)
                (no food and)
                (three baked potatoes)
                (3 diet hamburgers)))  ; ()
(intersectall '((3 mangoes and)
                ()
                (3 diet hamburgers)))  ; ()
;
; Even if one of the sets in lset is empty, intersectall keeps intersecting
; the empty set with some set until the list of sets is exhausted. 
;
; intersectall can be optimized to return '() once an empty set is found
; in lset using letcc or call-with-current-continuation.
;
; Define intersectall (using call-with-current-continuation)
;
; If an empty set is encoutered in lset, A hops to the right place and 
; returns '().
;
(define intersectall
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else (intersect (car lset) (A (cdr lset))))))))
          (cond
            ((null? lset) '())
            (else (A lset))))))))
;
; The same can be achieved using letcc. 
; letcc is not available in r5rs Scheme. But it can be implemented
; using macros.
;
; Define letcc
;
(define-syntax letcc 
  (syntax-rules () 
    ((letcc var body ...) 
     (call-with-current-continuation 
       (lambda (var)  body ... )))))
;
; Define intersectall (using letcc)
;
(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
        ((A (lambda (lset)
              (cond
                ((null? (car lset)) (hop '()))
                ((null? (cdr lset)) (car lset))
                (else (intersect (car lset) (A (cdr lset))))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))

; 
; ###############################################
;
;             The Fourteenth Commandment
;
;   Use (letcc ...) to return values abruptly
;   and promptly.
;
; ###############################################
;

; Define intersect (improved)
;
; This version of intersect returns '() immediately if set2 is
; empty, avoiding the process of traversing through set1.
;
(define intersect
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
            (cond
              ((null? set) '())
              ((member? (car set) set2) (cons (car set) (I (cdr set))))
              (else (I (cdr set)))))))
      (cond
        ((null? set2) '())
        (else (I set1))))))

; Define intersectall (improved)
;
; If none of sets in lset is '(), but if the intermediate result at any
; point is empty, then '() is immediately returned without intersecting
; with the remaining sets. Here, intersect is defined within the letrec
; block of intersectall to enable hopping using letcc.
;
(define intersectall
  (lambda (lset)
    (letcc hop
      (letrec
        ((A (lambda (lset)
              (cond
                ((null? (car lset)) (hop '()))
                ((null? (cdr lset)) (car lset))
                (else (I (car lset) (A (cdr lset)))))))
         (I (lambda (set1 set2)
              (letrec
                ((J (lambda (set)
                      (cond
                        ((null? set) '())
                        ((member? (car set) set2) (cons (car set) (J (cdr set))))
                        (else (J (cdr set)))))))
                (cond
                  ((null? set2) (hop '()))
                  (else (J set1)))))))
        (cond
          ((null? lset) '())
          (else (A lset)))))))

; Define rember-beyond-first
;
; If a occurs in the lat, it removes all atoms frrom the lat beyond
; and including the first occurrence of a.
;
(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) '())
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))
;
(rember-beyond-first 'roots
                     '(noodles spaghetti roots potatoes))  ; (noodles spaghetti)
(rember-beyond-first 'sweetthing
                     '(noodles spaghetti roots potatoes))  ; (noodles spaghetti roots potatoes)

; Define rember-upto-last
;
; Removes all the atoms from the lat up to and including the last
; occurrence of a. If there are no occurrences of a, it returns
; the lat.
;
; Upon discovering a, everything seen so far should be thrown away.
; This is different from intersectall in the sense that intersectall
; knows what the result is, rember-upto-last knows which pieces of the
; list are not in the result.
;
(define rember-upto-last
  (lambda (a lat)
    (letcc skip
      (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a) (skip (R (cdr lat))))
                (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))
;
(rember-upto-last 'roots
                  '(noodles roots spaghetti potatoes))  ; (spaghetti potatoes)
(rember-upto-last 'sweetthing
                  '(noodles roots spaghetti potatoes))  ; (noodles roots spaghetti potatoes)
