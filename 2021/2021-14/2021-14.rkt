#lang racket

#|
Apply 10 steps of pair insertion to the polymer template
and find the most and least common elements in the result.
What do you get if you take the quantity of the most common
element and subtract the quantity of the least common element?
|#

(require "../qtest.rkt")
(require threading)
(require algorithms)
(require srfi/26)

(define (read-data file)
  (with-input-from-file file
    (thunk
     (define template (string->list (read-line)))
     (read-line)
     (define rules
       (~>> (for/hash ((line (in-lines)))
              (define matches
                (regexp-match #px"(\\w\\w) -> (\\w)" line))
              (match-define (list _ key value) matches)
              (values (string->list key)
                      (string-ref value 0)))))
     (values template rules))))

(define (do-step polymer rules)
  (define pairs (sliding polymer 2 1))
  (define inserts (map (cute hash-ref rules <>)
                       pairs))
  (~>> inserts
       (append _ '(#f))
       (zip polymer _)
       (flatten _)
       (init _)))

(define-values (test-template test-rules)
  (read-data "test.txt"))
(qtest (do-step test-template test-rules)
       '(#\N #\C #\N #\B #\C #\H #\B))

(define (build-polymer template rules steps)
  (for/fold ((polymer template))
            ((i (in-range 0 steps)))
    (do-step polymer rules)))

(define (part1 file steps)
  (define-values (template rules)
    (read-data file))
  (define polymer (build-polymer template rules steps))
  (define elements (remove-duplicates polymer))
  #;(define counts
      (map (λ (element)
             (list element (count (cute char=? element <>)
                                  polymer)))
           elements))
  (define counts (map (λ (element)
                        (count (cute char=? element <>)
                               polymer))
                      elements))
  #;(writeln (sort counts >))
  (- (apply max counts)
     (apply min counts)))  

(qtest (part1 "test.txt" 10) 1588)
(qtest (part1 "input.txt" 10))
#;(qtest (part1 "input.txt" 40))

#|
(qtest (part1 "test.txt" 10))
(qtest (part1 "test.txt" 11))
(qtest (part1 "test.txt" 12))
(/ 1749 161)
(/ 7256 506)
|#

(define (foo steps-left counts rules pair)
  (cond ((= 0 steps-left)
         counts)
        (else
         (define sl (sub1 steps-left))
         (define new (hash-ref rules pair))
         (~> (hash-update counts new add1 0)
             (foo sl _ rules (list (first pair)
                                   new))
             (foo sl _ rules (list new (second pair)))))))

(define (part2 file steps)
  (define-values (template rules)
    (read-data file))
  (define pairs (sliding template 2 1))
  (define counts
    (~> (for/fold ((counts (hash)))
                  ((pair pairs))
          (~> counts
              (hash-update _ (first pair) add1 0)
              (hash-update _ (second pair) add1 0)
              (foo steps
                   _
                   rules
                   pair)))
        (hash-values _)))
  (- (apply max counts)
     (apply min counts)))

(qtest (part2 "test.txt" 10) 1588)
#;(qtest (part2 "test.txt" 40) 2188189693529)

(define (setup template)
  (values (for/fold ((ec (hash)))
                    ((element template))
            (hash-update ec element add1 0))
          (for/fold ((pc (hash)))
                    ((pair (sliding template 2 1)))
            (hash-update pc pair add1 0))))

(define (do-step-part2 rules element-counts pair-counts)
  #;(printf "total: ~s\n" (apply + (hash-values element-counts)))
  #;(printf "ec: ~s\npc: ~s\n\n" element-counts pair-counts)
  (for/fold ((ec element-counts)
             (pc (hash)))
            (((key value) pair-counts))
    (define new (hash-ref rules key))
    (define new-ec (hash-update ec new (cute + value <>) 0))
    (define first-pair (list (first key) new))
    (define second-pair (list new (second key)))
    (define new-pc
      (~> pc
          (hash-update _ first-pair (cute + value <>) 0)
          (hash-update _ second-pair (cute + value <>) 0)))
    (values new-ec new-pc)))

(define (part2-take2 file steps)
  (define-values (template rules)
    (read-data file))
  (define-values (ec pc)
    (setup template))
  (define-values (last-ec last-pc)
    (for/fold ((ec ec)
               (pc pc))
              ((step (in-range 0 steps)))
      (do-step-part2 rules ec pc)))
  (define counts (hash-values last-ec))
  (- (apply max counts)
     (apply min counts)))

(qtest (part2-take2 "test.txt" 10) 1588)
(qtest (part2-take2 "test.txt" 40) 2188189693529)
(qtest (part2-take2 "input.txt" 40))