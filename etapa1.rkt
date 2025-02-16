#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  ;'your-code-here
  (help '() w1 w2)
)

; auxiliar
(define (help pref w1 w2)
    (cond
      ; verificam daca nu sunt cuvintele vide
      ; + daca prima litera a fiecarui cuvant sunt identice
      ((and (not (null? w1)) (not (null? w2)) (equal? (car w1) (car w2)))
        ; #t adaugam in lista de prefixe
       (help (append pref (list (car w1))) (cdr w1) (cdr w2)))
      (else
        ; #f , gata prefixul, afisam listele generate
       (list pref w1 w2))))



; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  ;'your-code-here
  (define (common-prefix current-prefix r-words)
    (if (null? r-words)
        ; nu mai sunt cuvinte in lista
        ; intoarcem prefixul creat
        current-prefix
        ; apelam recursiv
        ; aflam prefixul comun intre 
        ; primul cuvant din lista ramasa si prefixul comun curent
        (common-prefix (car (longest-common-prefix current-prefix (car r-words)))
                   (cdr r-words))))
  (if (null? words)
      ; lista de cuvinte e nula
      '()
      (common-prefix (car words) (cdr words))))


;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)
  ;'your-code-here
  (define branch (get-ch-branch st (car pattern)))
  (cond
    ; nu exista o ramura care sa inceapa cu primul caracter din sablon
    ((not branch)       
        (list #f '()))
    ; sablon identic cu eticheta
    ((equal? (car (longest-common-prefix pattern (get-branch-label branch))) pattern)
        #t)  
    ; sablonul este prefix al etichetei
    ((prefix? (get-branch-label branch) pattern)
    ; afisam o lista cu eticheta, noul pattern si subarborele corespunzator
     (list (get-branch-label branch)
           (remove-prefix (get-branch-label branch) pattern)
           (get-branch-subtree branch)))
    ; Sablonul nu se potriveste cu eticheta
    (else                      
    (list #f (car(longest-common-prefix (get-branch-label branch) pattern))))))


(define (prefix? lst1 lst2)
  (if (null? lst1)
      #t
      (and (not (null? lst2))
           (equal? (car lst1) (car lst2))
           (prefix? (cdr lst1) (cdr lst2)))))
           


; Functia remove-prefix elimina prefixul comun dintre doua liste
(define (remove-prefix list1 list2)
  (cond
    ((null? list1) list2)
    ((null? list2) list1)
    ((equal? (car list1) (car list2))
     (remove-prefix (cdr list1) (cdr list2)))
    (else list1)))



; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (cond
    ; Potrivire exacta
    ((equal? (match-pattern-with-label st pattern) #t) #t)
    ; Nu se potriveste deloc
    ((equal? (car (match-pattern-with-label st pattern)) #f) #f) 
    ; Continuam cautarea recursiv cu noul pattern si subarborele corespunzator
    (else (st-has-pattern? (caddr (match-pattern-with-label st pattern)) (cadr (match-pattern-with-label st pattern))))))
