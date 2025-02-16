#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (st-has-pattern? (text->cst text) pattern))

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
         ; arborele de sufixe st1 pentru text1
  (let* ((st1 (text->cst text1)) 
         ; obtinem sufixele lui text2
         (suffixes (get-suffixes text2)))
        ; parcurgem toate sufixele 
        (let loop ((suffixes suffixes) (long '()))
                  ; daca nu mai sunt sufixe de verificat
                  (if (null? suffixes)
                      ; #t cel mai lung subsir
                      long
                      ; #f continuam cu urmatorul sufix
                      (let* ((suffix (car suffixes))
                             (rest (cdr suffixes))
                             ; cautam cel mai lung prefix comun
                             (match 
                                (let longest-prefix-st 
                                    ; intre arborele de sufixe si sufixul curent
                                    ((st st1) (pattern suffix) (acc '()))
                                    ; verificam daca sufixul se potriveste
                                    (let ((fit (match-pattern-with-label st pattern)))
                                         (if (equal? #t fit)
                                              ; #t adaugam la acc
                                             (append acc pattern)
                                             (if (equal? #f (car fit))
                                                 (append acc (cadr fit))
                                                 ; continuam cautarea cu restul arborelui de sufixe
                                                 (longest-prefix-st (caddr fit)
                                                                    ; restul sufixului 
                                                                    (cadr fit)
                                                                    (append acc (car fit))))))))
                             ; verificam daca noul match este mai lung decat cel mai lung gasit pana acum
                             (next-long 
                                (if (> (length match) (length long)) match long)))
                            ; continuam bucla cu restul sufixelor si noul cel mai lung
                            (loop rest next-long))))))

; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  (define (repeated st substring)
    (if (and (>= (length substring) len) (not (st-empty? st)))
        ; primele len elemente din substring
        (take substring len)
        ; arbore de sufixe sau prima ramura = null
        (if (or (null? st) (null? (first-branch st)))
            #f
            ; altfel, exploram prima ramura si celelalte ramuri
            (let ((first (first-branch st))
                  (other (other-branches st)))
                          ; prima ramura != null
                 (or (and (not (null? first))
                          ; apelam recursiv functia pe subarborele primei ramuri
                          (repeated (get-branch-subtree first)
                                    ; adaugam eticheta primei ramuri la substring 
                                    (append substring (get-branch-label first))))
                          ; prima ramura = null si celelalte nu
                     (and (not (null? other))
                          ; apeleam recursiv pe celelalte ramuri
                          (repeated other substring)))))))
  (repeated (text->cst text) '()))