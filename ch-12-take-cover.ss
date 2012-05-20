; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 12) Take Cover)
;
; Viswanath Sivakumar, aka viswanathgs <viswanathgs@gmail.com>
;

; Define atom?
;
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Define Y
;
; Y-Combinator
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x)))))))

; Define multirember (using Y)
;
; The function passed to Y does not take a as an argument. All references
; to a inside the function refer to the a in the outermost lambda.
;
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? a (car lat)) (mr (cdr lat)))
              (else (cons (car lat) (mr (cdr lat))))))))
     lat)))
;
(multirember 'tuna '(shrimp salad tuna salad and tuna)) ; (shrimp salad salad and)

; Define multirember (using letrec instead of Y)
;
; The letrec block (letrec ((mr ..)) mr) defines and returns a recursive function mr.
; ((letrec ((mr ..)) mr) lat) is the application of mr to lat.
;
(define multirember
  (lambda (a lat)
    ((letrec ((mr (lambda (lat)
                    (cond
                      ((null? lat) '())
                      ((eq? a (car lat)) (mr (cdr lat)))
                      (else (cons (car lat) (mr (cdr lat))))))))
             mr)
     lat)))
;
(multirember 'pie '(apple custard pie linzer pie torte)) ; (apple custard linzer torte)

; Define multirember (another version)
;
; The letrec block is modified to (letrec ((mr ..)) (mr lat)). The letrec block here
; does not return the function mr. Instead, mr is applied to lat inside the letrec
; block itself.
;
(define multirember
  (lambda (a lat)
    (letrec ((mr (lambda (lat)
                   (cond
                     ((null? lat) '())
                     ((eq? a (car lat)) (mr (cdr lat)))
                     (else (cons (car lat) (mr (cdr lat))))))))
            (mr lat))))

; 
; ###############################################
;
;              The Twelfth Commandment
;
;    Use (letrec ...) to remove arguments that
;    do not change for recursive applications.
;
; ###############################################
;

; Define multirember-f
;
(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

; Define multirember-f (using letrec)
;
; In multirember-f, the result of (multirember-f test?) is always the same
; throughout a traversal. Hence, it is renamed using letrec.
;
(define multirember-f
  (lambda (test?)
    (letrec ((m-f (lambda (a lat)
                    (cond
                      ((null? lat) '())
                      ((test? a (car lat)) (m-f a (cdr lat)))
                      (else (cons (car lat) (m-f a (cdr lat))))))))
            m-f)))

; Define member? (using twelfth commandment)
;
(define member?
  (lambda (a lat)
    ((letrec ((yes? (lambda (l)
                      (cond
                        ((null? l) #f)
                        (else (or (eq? a (car l)) (yes? (cdr l))))))))
             yes?)
     lat)))
;
(member? 'ice '(salad greens with pears brie cheese frozen yogurt)) ; #f
;
; Define member? (another version)
;
(define member?
  (lambda (a lat)
    (letrec ((yes? (lambda (l)
                    (cond
                      ((null? l) #f)
                      (else (or (eq? a (car l)) (yes? (cdr l))))))))
            (yes? lat))))

; Define union (from The Little Schemer)
;
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))
;
(union '(tomatoes and macaroni casserole)
       '(macaroni and cheese)) ; (tomatoes casserole macaroni and cheese)
;
; The value of set2 always stays the same when determining 
; the value of (union set1 set2). 
;
; Define union (using twelfth commandment)
;
(define union
  (lambda (set1 set2)
    (letrec ((U (lambda (set)
                  (cond
                    ((null? set) set2)
                    ((member? (car set) set2) (U (cdr set)))
                    (else (cons (car set) (U (cdr set))))))))
            (U set1))))
;
; The function union depends on member?. If, for example, the order
; of arguments in member? is changed, that change should also be 
; reflected in the call to member? inside the definition of union.
; To protect member?, it can be defined inside the letrec part of
; union.
;
; Define union (with member? inside letrec)
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2) (U (cdr set)))
              (else (cons (car set) (U (cdr set)))))))
       (M? (lambda (a lat)
             (cond
               ((null? lat) #f)
               (else (or (eq? a (car lat)) (M? a (cdr lat))))))))
      (U set1))))

;
; ###############################################
;
;            The Thirteenth Commandment
;
;    Use (letrec ...) to hide and to protect
;    functions.
;
; ###############################################
;

; Define union (revised)
;
; The member? (M?) inside the letrec block of union does 
; not follow the twelfth commandment. This definition fixes
; that.
;
(define union
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2) (U (cdr set)))
              (else (cons (car set) (U (cdr set)))))))
       (M? (lambda (a lat)
             (letrec
               ((N? (lambda (lat)
                      (cond
                        ((null? lat) #f)
                        (else (or (eq? a (car lat)) (N? (cdr lat))))))))
               (N? lat)))))
      (U set1))))

; Define two-in-a-row? (from chapter 11)
;
; Using letrec to hide two-in-a-row-b?
;
(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else (or (eq? a (car lat)) (W (car lat) (cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))
;
; In the above function, W does not need to know the arguments
; in the outer lambda. So, their order can be interchanged.
;
; Define two-in-a-row? (another version)
;
; Here, letrec returns a function that takes a lat as an 
; argument.
;
(define two-in-a-row?
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat)) (W (car lat) (cdr lat))))))))
    (lambda (lat) 
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

; Define sum-of-prefixes (from chapter 11)
;
; Using letrec to hide sum-of-prefixes-b.
;
(define sum-of-prefixes
  (lambda (tup)
    (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) '())
              (else (cons (+ sss (car tup)) (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))
;
; Define sum-of-prefixes (another version)
;
(define sum-of-prefixes
  (letrec
    ((S (lambda (sss tup)
          (cond 
            ((null? tup) '())
            (else (cons (+ sss (car tup)) (S (+ sss (car tup)) (cdr tup))))))))
    (lambda (tup) (S 0 tup))))

; Define pick (from chapter 04 of The Little Schemer)
;
(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else (pick (- n 1) (cdr lat))))))

; Define scramble (from chapter 11)
;
; Using letrec to protect scramble-b.
;
(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (tup rp)
            (cond
              ((null? tup) '())
              (else
                (cons (pick (car tup) (cons (car tup) rp))
                      (P (cdr tup) (cons (car tup) rp))))))))
      (P tup '()))))
;
; Define scramble (another version)
;
(define scramble
  (letrec
    ((P (lambda (tup rp)
          (cond
            ((null? tup) '())
            (else 
              (cons (pick (car tup) (cons (car tup) rp))
                    (P (cdr tup) (cons (car tup) rp))))))))
    (lambda (tup) (P tup '()))))
