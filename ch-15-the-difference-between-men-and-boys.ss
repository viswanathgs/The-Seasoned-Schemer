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

; set!
;
(define x 'bang)
x ; bang
(set! x 'gone)
x ; gone
(set! x 'skins)
x ; skins
;
(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))
;
(gourmet 'onion) ; (onion skins)
(set! x 'rings)
(gourmet 'onion) ; (onion rings)
;
(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))
;
(gourmand 'potato) ; (potato potato)
x ; potato
(gourmand 'rice); (rice rice)
x ; rice
;
(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake 
          (cons food '()))))
;
(dinerR 'onion) ; (milkshake onion)
x ; onion
(dinerR 'pecanpie) ; (milkshake pecanpie)
x ; pecanpie
;
; gourmand and dinerR, both refer to the same x, which is defined outside.
;
(define omnivore
  (let ((x 'minestrone))
       (lambda (food)
         (set! x food)
         (cons food
               (cons x '())))))
;
(omnivore 'bouillabaisse) ; (bouillabaisse bouillabaisse)
x ; pecanpie

;
; ###############################################
;
;             The Sixteenth Commandment
;
;   Use (set! ...) only with names defined in
;   (let ...)s.
;
; ###############################################
;

(define gobbler
  (let ((x 'minestrone))
       (lambda (food)
         (set! x food)
         (cons food
           (cons x '())))))
;
; omnivore and gobbler don't refer to the same x.
;

; 
; ###############################################
;
;            The Seventeenth Commandment
;
;               (preliminary version)
;
;   Use (set! x ...) for (let ((x ...)) ...) 
;   only if there is at least one (lambda ...
;   between it and the (let ((x ...)) ...).
;
; ############################################### 
;

(define food 'none)
;
(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x '()))))))
;
(glutton 'garlic) ; (more garlic more garlic)
food ; garlic

; 
; ###############################################
;
;             The Eighteenth Commandment
;
;   Use (set! x ...) only when the value that x
;   refers to is no longer needed.
;
; ###############################################
;

; Define chez-nous
;
; Swaps what x and food refer to.
;
(define chez-nous
  (lambda ()
    (let ((a food))
         (set! food x)
         (set! x a))))
;
(glutton 'garlic) ; (more garlic more garlic)
food ; garlic
(gourmand 'potato) ; (potato potato)
x ; potato
(chez-nous)
food ; potato
x ; garlic
