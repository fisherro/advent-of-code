#lang racket

(require threading)
(require srfi/26)
(require "../qtest.rkt")

; I'm going to use symbols to represent the signals.
; This takes "abc" and returns '(a b c).
(define (string->signal-list s)
  (map (compose1 string->symbol string)
       (string->list s)))

; When comparing lists of symbols, order doesn't matter.
; So we'll need to sort them before comparing.
(define (sort-signals sigs)
  (sort sigs symbol<?))

(define (string->sorted-signal-list s)
  (compose1 sort-signals string->signal-list))

; A table from lists of symbols to the corresponding digit.
(define signals->digit-hash
  (hash '(a b c e f g)   0
        '(c f)           1
        '(a c d e g)     2
        '(a c d f g)     3
        '(b c d f)       4
        '(a b d f g)     5
        '(a b d e f g)   6
        '(a c f)         7
        '(a b c d e f g) 8
        '(a b c d f g)   9))

; All the permutations of the signals.
(define permutations-list (permutations '(a b c d e f g)))

; Given a list of signals & a permutation,
; translat the list into that permutation.
(define (translate-signals the-permutation signals)
  (define (xlate sig)
    (list-ref '(a b c d e f g)
              (index-of the-permutation sig)))
  (~> (map xlate signals)
      (sort-signals _)))

; Check to see if a pattern translated into a permutation gives a valid digit.
(define (permutation-of-pattern-valid? the-permutation pattern)
  (~> (translate-signals the-permutation pattern)
      (hash-has-key? signals->digit-hash _)))

; Check to see if all the patterns are valid for a permutation.
(define (test-permutation the-permutation patterns)
  (andmap (cute permutation-of-pattern-valid? the-permutation <>)
          patterns))

; Find the permutation that is valid for the patterns.
(define (find-permutation patterns)
  (findf (cute test-permutation <> patterns)
         permutations-list))

; Return a list of entries.
; Each entries is a list of a list of patterns and a list of outputs.
; Patterns & outputs are lists of sorted symbols.
(define (read-data file)
  (with-input-from-file file
    (thunk
     (for/list ((line (in-lines)))
       (~> (string-split line " | ")
           (map (cute string-split <> " ")
                _)
           (map (λ (part)
                  (map string->signal-list part))
                _))))))

; Try all permutations on the patterns of the entry.
; When a good permutation is found, translate the output according to it.
(define (unscramble-entry entry)
  (match-define (list patterns output) entry)
  (define matching-permutation (find-permutation patterns))
  (~> (map (cute translate-signals matching-permutation <>) output)
      (map (cute hash-ref signals->digit-hash <>) _)
      (map number->string _)
      (apply string-append _)
      (string->number _)))

(define (part2 file)
  (~> (read-data file)
      (map unscramble-entry _)
      (apply + _)))

(qtest (part2 "test1.txt") 5353)
(qtest (part2 "test.txt") 61229)
(qtest (part2 "input.txt"))
