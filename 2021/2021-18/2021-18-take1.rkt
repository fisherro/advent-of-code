#lang racket

(require "../qtest.rkt")
(require srfi/26)
(require anaphoric)
(require threading)
(require algorithms)

(define (wl x)
  (writeln x)
  x)

; (a(b(c(d(e f)g)h)i)j)
; a-d,g-j are list of zero or 1 elements
(define (match-explosion sn)
  (match sn
    ((list (or (? list? a) (? number? a)) ...
           (list (or (? list? b) (? number? b)) ...
                 (list (or (? list? c) (? number? c)) ...
                       (list (or (? list? d) (? number? d)) ...
                             (list (? number? e) (? number? f))
                             (or (? list? g) (? number? g)) ...)
                       (or (? list? h) (? number? h)) ...)
                 (or (? list? i) (? number? i)) ...)
           (or (? list? j) (? number? j)) ...)
     (hash 'a a 'b b 'c c 'd d 'e e 'f f 'g g 'h h 'i i 'j j))
    (else
     #f)))

#|
(match-explosion '(1 2))
(match-explosion '(((((9 8) 1) 2) 3) 4))
(match-explosion '[7 [6 [5 [4 [3 2]]]]])
(match-explosion '[[6 [5 [4 [3 2]]]] 1])
|#

;(((((2 3) 4) 5) 6))

(define (add-to-rightmost to-update to-add)
  (cond ((number? to-update)
         (+ to-update to-add))
        (else
         (append (init to-update)
                 (list (add-to-rightmost (second to-update)
                                         to-add))))))
(add-to-rightmost '((((2 3) 4) 5) 6) 3)

(define (update-matches hsh source dest-list)
  (define source-value (hash-ref hsh source))
  (define head (first dest-list))
  (define tail (rest dest-list))
  (define head-value (hash-ref hsh head))
  (cond ((number? head-value)
         (hash-update hsh head (cute + <> source-value)))
        ((and (list? head-value)
              (not (empty? head-value)))
         (hash-update hsh head (cute add-to-rightmost <> source-value)))
        ((empty? tail)
         hsh)
        (else
         (update-matches hsh source tail))))

(define (fixup-list lst)
  (~> (map (λ (element)
             (cond ((list? element)
                    (fixup-list element))
                   (else element)))
           lst)
      (filter (λ (element)
                (not (and (list? element)
                          (empty? element))))
              _)))

(define (matches->list hsh)
  (~> (list (hash-ref hsh 'a)
            (list (hash-ref hsh 'b)
                  (list (hash-ref hsh 'c)
                        (list (hash-ref hsh 'd)
                              0
                              (hash-ref hsh 'g))
                        (hash-ref hsh 'h))
                  (hash-ref hsh 'i))
            (hash-ref hsh 'j))
      (fixup-list _)))

(define (fixup-matches matches)
  (for/hash (((k v) (in-hash matches)))
    (values k (match v
                ((list (? number? n)) n)
                (else v)))))

(define (do-explosion sn)
  (acond ((match-explosion sn)
          (~> it
              (wl _)
              (fixup-matches _)
              (update-matches _ 'e '(d c b a))
              (update-matches _ 'f '(g h i j))
              (matches->list _)))
         (else sn)))

(define (recursive-reverse lst)
  (~> (map (λ (element)
             (cond ((list? element)
                    (recursive-reverse element))
                   (else element)))
           lst)
      (reverse _)))

(recursive-reverse '[[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]])

#;(qtest (do-explosion '(((((9 8) 1) 2) 3) 4))
       '[[[[0 9] 2] 3] 4])
#;(qtest (do-explosion '[7 [6 [5 [4 [3 2]]]]])
       '[7 [6 [5 [7 0]]]])
#;(qtest (do-explosion '[[6 [5 [4 [3 2]]]] 1])
       '[[6 [5 [7 0]]] 3])
(qtest (do-explosion '[[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]])
       '[[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]])
(qtest (do-explosion (recursive-reverse '[[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]]))
       (recursive-reverse '[[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]]))
#;(qtest (do-explosion '[[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]])
       '[[3 [2 [8 0]]] [9 [5 [7 0]]]])

(define (sn-reduce sn) #f)

;  (match p
;    ((cons (? number? a) (? number? b))
;     #t))

(define (sn-split n)
  (define half (/ n 2))
  (cons (floor half)
        (ceiling half)))

(define (sn+ a b)
  (cons a b))