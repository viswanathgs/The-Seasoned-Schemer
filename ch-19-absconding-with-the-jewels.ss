; 
; Code snippets from "The Seasoned Schemer", by
; Daniel P. Friedman and Matthias Felleisen 
; 
; ((Chapter 19) Absconding with the Jewels)
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

; Define deepB
;
; Similar to deep, but can change the value of the atom without executing the entire
; function again.
;
; topppings remembers where to jump.
;
(define toppings)
;
(define deepB
  (lambda (m)
    (cond
     ((zero? m)
      (letcc jump
             (set! toppings jump)
             'pizza))
     (else (cons (deepB (- m 1)) '())))))
;
(deepB 3) ; (((pizza)))
;
; toppings now remembers jump
;
(toppings 'mozzarella) ; (((mozzarella)))
(toppings 'cake) ; (((cake)))
;
(cons (toppings 'cake) '()) ; (((cake)))
(cons
 (cons
  (cons (toppings 'mozzarella) '())
  '())
 '()) ; (((mozzarella)))
;
; In the above two examples, the cons surrounding toppings is forgotten. We
; directly hop to the location of jump and execute the rest of that function.
;

;
; ###############################################
;
;             The Twentieth Commandment
;
;   When thinking about a value created with
;   (letcc ...), write down the function that is
;   equivalent but does not forget. Then, when
;   you use it, remember to forget.
;
; ###############################################
;

;
(deepB 5) ; (((((pizza)))))
(cons
 (cons
  (cons (toppings 'cake) '())
  '())
 '()) ; (((((cake)))))