#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).

(define (longest-common-prefix w1 w2)
  (define (help pref w1 w2)
    (cond
      ((and (not (null? w1)) (not (null? w2)) (equal? (car w1) (car w2)))
       (help (cons (car w1) pref) (cdr w1) (cdr w2)))
      (else
       (list (reverse pref) w1 w2))))
  (help '() w1 w2))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (define (common-prefix current-prefix r-words)
    (if (collection-empty? r-words)
        ; nu mai sunt cuvinte in lista
        ; intoarcem prefixul creat
        current-prefix
        ; apelam recursiv
        ; aflam prefixul comun intre 
        ; primul cuvant din lista ramasa si prefixul comun curent
        (common-prefix (collection-first (longest-common-prefix current-prefix (collection-first r-words)))
                       (collection-rest r-words))))
  (if (null? words)
      ; lista de cuvinte e nula
      '()
      (common-prefix (collection-first words) (collection-rest words))))


(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern)))
         (branch-label (if branch (get-branch-label branch) '()))
         (common-prefix (longest-common-prefix pattern branch-label))
         (new-pattern (remove-prefix branch-label pattern)))
    (cond
      ((not branch) (list #f '())) ; nu exista o ramura care sa inceapa cu primul caracter din sablon
      ((equal? (car common-prefix) pattern) #t) ; sablon identic cu eticheta
      ((prefix? branch-label pattern) ; sablonul este prefix al etichetei
       (list branch-label new-pattern (get-branch-subtree branch))) ; afisam o lista cu eticheta, noul pattern si subarborele corespunzator
      (else (list #f (car common-prefix)))))) ; Sablonul nu se potriveste cu eticheta


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

(define (st-has-pattern? st pattern)
  (let ((match-result (match-pattern-with-label st pattern)))
    (cond
      ; Potrivire exacta
      ((equal? match-result #t) #t)
      ; Nu se potriveste deloc
      ((equal? (stream-first match-result) #f) #f) 
      ; Continuam cautarea recursiv cu noul pattern si subarborele corespunzator
      (else (st-has-pattern? (stream-ref match-result 2) (stream-ref match-result 1))))))

; etapa 2
(define (get-suffixes text)
  ; Verificam daca textul este vid
  (if (null? text)
    ; intoarcem lista vida, deoarece nu exista sufixe
    '() 
    ; adaugam textul curent la începutul listei rezultat 
    ; si apelam recursiv functia pentru restul textului
    (collection-cons text (get-suffixes (cdr text)))))


(define (get-ch-words words ch)
  ; pentru fiecare cuvant din stream-ul de cuvinte
  (collection-filter (lambda (word)
    ; verificam cuvantul sa nu fie null
    ; si primul caracter al cuvantului = ch
    (and (not (collection-empty? word))
         (char=? (collection-first word) ch)))
  ; aplicam lambda fiecarui cuvant din stream
  words))



(define (ast-func suffixes)
  ; adaugam intr-o lista prima litera a primului sufix
  (cons (list (collection-first (collection-first suffixes)))
    ; elimina prima litera din fiecare sufix
    (collection-map stream-rest suffixes)))



(define (cst-func suffixes)
  ; Folosim let* pentru a calcula o singură dată prefixul comun cel mai lung (LCP) și lungimea acestuia
  (let* (
         ; Calculăm LCP din colecția de sufixe
         (lcp (longest-common-prefix-of-collection suffixes))
         ; Calculăm lungimea LCP
         (lcp-length (length lcp))
        )
    ; Folosim cons pentru a crea o pereche unde primul element este LCP
    ; iar al doilea element este rezultatul mapării unei funcții peste sufixe
    (cons 
      lcp
      ; Mapăm o funcție peste sufixe care elimină LCP din fiecare sufix
      (collection-map 
        (lambda (suffix)
          ; Folosim drop pentru a elimina LCP din sufix
          (drop suffix lcp-length))
        suffixes))))

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  ; subarbore pentru un caracter dat
  (define (subtree ch)
    ; 
    ((lambda (ch-words)
      ; verificam daca lista de sufixe e goala
      (if (stream-empty? ch-words)
        ; returneaza vid/null
        '()
        ; construim un arbore, pentru fiecare sufix
        (cons (car (labeling-func ch-words))
          ; construim recursiv subarborele pentru sufixele ramase
          (suffixes->st labeling-func
          ; eliminam sufixul curent din lista 
          (cdr (labeling-func ch-words))
          alphabet))))
    ; obtinem lista de sufixe care incep cu ch
    (get-ch-words suffixes ch)))
  
  ; verificam daca alfabetul este gol
  (if (stream-empty? alphabet)
    ; alfabet gol => arbore vid
    '()
    ; construim subarborii pentru fiecare caracter din alfabet 
    ; si ii adunam intr-o lista
    ; subtree pe fiecare caracter din alfabet și eliminam arborele vid rezultat
    ; filter - filtreaza caracterele din alfabet care nu genereaza subarbori ne-vidi
    (collection-map subtree (collection-filter (lambda (ch) (not (null? (subtree ch)))) alphabet))))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (labeling-func)
    (lambda (text)
      (suffixes->st 
        labeling-func 
        ; adaugam la text $
        (get-suffixes (append text '(#\$))) 
        ; creem alfabetul
        ; sortam, stergem duplicatele
        (list->stream (sort (cons #\$ (remove-duplicates text)) char<?))))))

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

(define text->ast
  ((curry text->st) ast-func))


(define text->cst
  ((curry text->st) cst-func))


;etapa3
; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (st-has-pattern? (text->ast text) pattern))



; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (define (repeated st substring)
    (if (and (>= (length substring) len) (not (st-empty? st)))
        ; primele len elemente din substring
        (take substring len)
        ; arbore de sufixe sau prima ramura = null
        (if (or (st-empty? st) (null? (first-branch st)))
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
                     (and (not (st-empty? other))
                          ; apeleam recursiv pe celelalte ramuri
                          (repeated other substring)))))))
  (repeated (text->cst text) '()))