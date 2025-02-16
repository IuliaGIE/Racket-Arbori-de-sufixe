#lang racket

(provide (all-defined-out))

;; În acest fișier vă definiți constructorii și
;; operatorii tipului Collection.
;; În etapele anterioare, colecțiile erau de fapt
;; liste.
;; În definițiile de mai jos, veți considera că
;; o colecție este implementată ca flux.

; Întrucât stream-cons nu este o funcție obișnuită, 
; ci este definită ca o sintaxă specială, astfel
; încât ea să nu își evalueze argumentele înainte 
; de apel (comportament pe care ni-l dorim și pentru 
; collection-cons), nu putem folosi o definiție
; de tipul
;    (define collection-cons stream-cons)
; (genul acesta de definiție generează o eroare).
; Nici varianta
;    (define (collection-cons x xs) (stream-cons x xs))
; nu este o soluție, întrucât funcțiile definite de noi
; în Racket sunt funcții stricte, iar x și xs vor fi
; evaluate înainte de a intra în corpul funcției
; collection-cons și a descoperi că ele vor fi
; argumentele unui stream-cons.
; Modul de a defini collection-cons pentru a reproduce
; întocmai comportamentul lui stream-cons este:
(define-syntax-rule (collection-cons x xs) (stream-cons x xs))
; Obs: puteți schimba numele funcției, dacă nu vă
; place "collection-cons". Este o funcție folosită doar
; de voi în fișierul etapa4.rkt, nu de checker.


; TODO
; Scrieți în continuare restul definițiilor
; (care nu necesită o sintaxă specială).

;; Funcții și valori predefinite pe stream-uri:
;; * stream-cons
;; * stream-first
;; * stream-rest
;; * empty-stream
;; * stream-empty?
;; * stream-map, stream-filter
(define-syntax-rule (collection-first x) (stream-first x))
(define-syntax-rule (collection-rest x) (stream-rest x))
;(define-syntax-rule empty-collection empty-stream)
(define empty-collection empty-stream)
(define-syntax-rule (collection-empty? x) (stream-empty? x))
(define-syntax-rule (collection-map proc s) (stream-map proc s))
(define-syntax-rule (collection-filter pred s) (stream-filter pred s))
(define-syntax-rule (collection-fold proc init s) (stream-fold proc init s))
(define-syntax-rule (collection-append x xs) (stream-append x xs))

