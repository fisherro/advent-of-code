#lang racket

(require "../qtest.rkt")
(require srfi/26)
(require threading)

(define (close? c)
  (case c
    ((#\) #\] #\} #\>)
     #t)
    (else
     #f)))

(define (match? close open)
  (define the-match
    (case close
      ((#\)) #\()
      ((#\]) #\[)
      ((#\}) #\{)
      ((#\>) #\<)))
  (char=? the-match open))

(define (score c)
  (case c
    ((#\)) 3)
    ((#\]) 57)
    ((#\}) 1197)
    ((#\>) 25137)
    (else 0)))

(define (find-incorrect-close line)
  (define mismatch
    (for/fold ((stack '()))
              ((c line))
      #:break (char? stack)
      (cond ((close? c)
             (cond ((match? c (first stack))
                    (rest stack))
                   (else
                    ; Because stack will now be a char, we'll break out
                    c)))
            (else
             (cons c stack)))))
  (if (char? mismatch)
      mismatch
      #f))

(define (part1 file)
  (with-input-from-file file
    (thunk
     (~> (in-lines)
         (sequence->list _)
         (map find-incorrect-close _)
         (map score _)
         (apply + _)))))

(qtest (part1 "test.txt") 26397)
(qtest (part1 "input.txt"))

(define (incomplete? line)
  (not (find-incorrect-close line)))

; I could've had one procedure to both reject corrupt lines & complete line.
; I didn't.
; There's no need to flip the results to the matching close symbols.
; We'll just score based on the open symbols instead.
(define (complete line)
  (for/fold ((stack '()))
            ((c line))
    (cond ((close? c)
           (rest stack))
          (else
           (cons c stack)))))

; We're scoring open symbols because we didn't bother mapping to close symbols.
(define (autocomplete-points c)
  (case c
    ((#\() 1)
    ((#\[) 2)
    ((#\{) 3)
    ((#\<) 4)))

; Completition is a list of the open symbols that we need to close.
(define (autocomplete-score completion)
  (for/fold ((score 0))
            ((c completion))
    (+ (autocomplete-points c)
       (* score 5))))

(define (get-middle lst)
  (list-ref lst (quotient (sub1 (length lst))
                          2)))

(define (part2 file)
  (with-input-from-file file
    (thunk
     (~> (in-lines)
         (sequence->list _)
         (filter incomplete? _)
         (map complete _)
         (map autocomplete-score _)
         (sort _ <)
         (get-middle _)))))

(qtest (part2 "test.txt") 288957)
(qtest (part2 "input.txt"))