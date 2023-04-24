#lang racket

(require "etapa2.rkt")

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția


; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)   
(let loop ((eng engagements) (unstable-couples '()))  ;; in unstable-couples baga cuplurile instabile
    (cond
      ((null? eng) unstable-couples)
      ((better-match-exists? (caar eng) (cdar eng) (get-pref-list wpref (caar eng)) mpref engagements) ;;engagements e lista toata, eng lista actuala care e iterata
       (loop (cdr eng) (cons (cons (caar eng) (cdar eng))                        ;; verificam nu doar daca femeia e infidela ci si barbatul 
                                     unstable-couples)))
      ((better-match-exists? (cdar eng) (caar eng) (get-pref-list mpref (cdar eng)) wpref      
         (cons (cons (cdar eng) (caar eng))
               (remove (cons (caar eng) (cdar eng)) engagements)))  ;;bagam perechea pe invers in timp ce o scoatem pentru a avea un reverse de engagements practic
       (loop (cdr eng) (cons (cons (caar eng) (cdar eng))
                             unstable-couples)))
      (else (loop (cdr eng) unstable-couples)))))



; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let loop ((freem free-men)
             (engs engagements)
             (womens-of-man (get-pref-list mpref (car free-men)))
             (is-changed #f))
    (if (null? freem)
        engs
        (let ((first-ungaged-man (car freem))
              (women-partner (get-partner engs (car womens-of-man))))
          (if (equal? women-partner #f) ;;femeia n-are partener
              (if (= (length freem) 1)   ;;daca e ultimul barbat liber si are femeie fara partener o adauga direct 
                  (cons (cons (car womens-of-man) first-ungaged-man) engs)
                  (loop (cdr freem)
                        (cons (cons (car womens-of-man) first-ungaged-man) engs)
                        (get-pref-list mpref
                                       (if is-changed   ;;verifica daca s-a produs o inerschimbare intre parteneri adica daca w il prefera pe m lui m'
                                           (car freem)
                                           (cadr freem)))
                        #f)) ;;pana aici totul bine 100 la suta
                  (if (not (equal? women-partner first-ungaged-man))  ;;daca partenerul femeii din preferinte ale lui m nu este egal cu m'
                      (let ((women-current-partner (get-partner engs (car womens-of-man)))
                            (women-pref-list (get-pref-list wpref (car womens-of-man))))
                        (if (preferable? women-pref-list first-ungaged-man women-current-partner) ;;daca prefera femeia pe barbatul liber in loc de cel pe care il are
                            (let ((updated-free-man (cons women-current-partner (cdr freem))))  ;;updaeaza lista barbatilor liberi
                              (loop updated-free-man
                                    (update-engagements engs (car womens-of-man) first-ungaged-man)  ;;updateaza lista de logodne cu barbatul pe care il prefera 
                                    (get-pref-list mpref women-current-partner)
                                    #t))   ;;se produce interschimbarea
                            (loop freem
                                  engs
                                  (cdr womens-of-man)
                                  #f)))
                      (loop (cdr freem)
                            engs
                            (get-pref-list mpref (cdr freem))
                            is-changed)))))))
                     

; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
   (engage (get-men mpref) '() mpref wpref))



; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
    (foldr (lambda (pair members)
                (cons (car pair) (cons (cdr pair) members)))
              '()
              pair-list))

