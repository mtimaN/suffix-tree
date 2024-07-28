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
  (let ([st (text->ast text)])
    (st-has-pattern? st pattern)))


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

(define (check-max suf-iter suffixes result acc)
  (if (< (length result) (length acc))
      (suf-iter (cdr suffixes) acc)
      (suf-iter (cdr suffixes) result)))

(define (longest-common-substring text1 text2)
  (let ([st (text->ast (append text1 (list #\$)))] [suffixes (get-suffixes text2)])
    (let suf-iter ([suffixes suffixes] [result `()])
      (if (null? suffixes)
          (reverse result)
          (let st-search ([st st] [pattern (car suffixes)] [acc `()])
            (let ([match-result (match-pattern-with-label st pattern)])
              (cond
                ((equal? match-result #t) (check-max suf-iter suffixes result (cons (car pattern) acc)))
                ((equal? (car match-result) #f)
                 (if (null? (cadr match-result))
                     (check-max suf-iter suffixes result acc)
                     (check-max suf-iter suffixes result (append acc (cdr match-result)))))
                (else
                 (st-search (caddr match-result) (cadr match-result) (cons (caar match-result) acc))))))))))
                  
                   
          


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
(define (choose-branch x y)
  (cond
    ((pair? x) x)
    ((pair? y) y)
    (else
     #f)))

(define (repeated-substring-of-given-length text len)
  (let ([st (text->cst text)])
    (let branch-iter ([st st] [acc `()])
      (if (st-empty? st)
          #f
          (let ([child (get-branch-subtree (first-branch st))]
                [new-acc (append acc (get-branch-label (first-branch st)))])
            (cond
              ((and (pair? child) (>= (length new-acc) len)) (take new-acc len))
              (else
               (let ([child-search (branch-iter child new-acc)]
                     [brother-search (branch-iter (other-branches st) acc)])
                 (choose-branch child-search brother-search)))))))))
           
