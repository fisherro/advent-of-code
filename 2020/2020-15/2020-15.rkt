#lang racket

(require anaphoric)
(require srfi/26)

(define (wl x)
  (writeln x)
  x)

; A quick unit test function:
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(define (expect a b)
  (define result (eval a ns))
  (printf "[~a] \"~s\" returned ~a expected ~a\n"
          (if (equal? result b)
              "PASS"
              "FAIL")
          a
          result
          b))

(define (find-next-reverse seq)
  (define last (first seq))
  (define pos (index-of (rest seq)
                        last))
  (cond (pos
         (add1 pos))
        (else
         0)))

(define (find-next seq)
  (find-next-reverse (reverse seq)))

; We can't have already added last-value to seen...
(define (find-next2 turn last-value seen)
  #;(printf "find-next2 turn = ~s last-value = ~s seen = ~s\n"
          turn last-value seen)
  (define last-turn (hash-ref seen last-value #f))
  (cond (last-turn
         (sub1 (- turn last-turn)))
        (else
         0)))

(expect '(find-next '(0 3 6)) 0)
(expect '(find-next '(0 3 6 0)) 3)
(expect '(find-next '(0 3 6 0 3)) 3)
(expect '(find-next '(0 3 6 0 3 3)) 1)
(expect '(find-next '(0 3 6 0 3 3 1)) 0)
(expect '(find-next '(0 3 6 0 3 3 1 0)) 4)
(expect '(find-next '(0 3 6 0 3 3 1 0 4)) 0)

(define (find-turn last-turn seq-start)
  (let loop ((seq (reverse seq-start))
             (turn (add1 (length seq-start))))
    (let ((next (find-next-reverse seq)))
      #;(printf "Turn ~a: ~s\n" turn seq)
      (if (= turn last-turn)
          next
          (loop (cons next seq)
                (add1 turn))))))

(define (find-turn2 last-turn seq-start)
  (define (init-seen seq)
    (let loop ((seq (drop-right seq 1))
               (seen (hash))
               (turn 1))
      #;(printf "init-seen: seen = ~s\n" seen)
      (if (null? seq)
          seen
          (loop (rest seq)
                (hash-set seen
                          (first seq)
                          turn)
                (add1 turn)))))
  (let loop ((seen (init-seen seq-start))
             (turn (add1 (length seq-start)))
             (last-value (last seq-start)))
    #;(printf "find-turn2: seen = ~s; turn = ~s\n" seen turn)
    (let ((next (find-next2 turn last-value seen)))
      (if (= turn last-turn)
          next
          (loop (hash-set seen last-value (sub1 turn))
                (add1 turn)
                next)))))

(expect '(find-turn 4 '(0 3 6)) 0)
(expect '(find-turn 5 '(0 3 6)) 3)
(expect '(find-turn 6 '(0 3 6)) 3)
(expect '(find-turn 7 '(0 3 6)) 1)
(expect '(find-turn 8 '(0 3 6)) 0)
(expect '(find-turn 9 '(0 3 6)) 4)
(expect '(find-turn 10 '(0 3 6)) 0)

(expect '(find-turn2 4 '(0 3 6)) 0)
(expect '(find-turn2 5 '(0 3 6)) 3)
(expect '(find-turn2 6 '(0 3 6)) 3)
(expect '(find-turn2 7 '(0 3 6)) 1)
(expect '(find-turn2 8 '(0 3 6)) 0)
(expect '(find-turn2 9 '(0 3 6)) 4)
(expect '(find-turn2 10 '(0 3 6)) 0)

(expect '(find-turn 2020 '(0 3 6)) 436)

(expect '(find-turn 2020 '(1 3 2)) 1)
(expect '(find-turn 2020 '(2 1 3)) 10)
(expect '(find-turn 2020 '(1 2 3)) 27)
(expect '(find-turn 2020 '(2 3 1)) 78)
(expect '(find-turn 2020 '(3 2 1)) 438)
(expect '(find-turn 2020 '(3 1 2)) 1836)

(expect '(find-turn 2020 '(8 13 1 0 18 9)) 755)

(expect '(find-turn2 2020 '(0 3 6)) 436)

(expect '(find-turn2 2020 '(1 3 2)) 1)
(expect '(find-turn2 2020 '(2 1 3)) 10)
(expect '(find-turn2 2020 '(1 2 3)) 27)
(expect '(find-turn2 2020 '(2 3 1)) 78)
(expect '(find-turn2 2020 '(3 2 1)) 438)
(expect '(find-turn2 2020 '(3 1 2)) 1836)

(expect '(find-turn2 2020 '(8 13 1 0 18 9)) 755)

(expect '(find-turn2 30000000 '(0 3 6)) 175594)

(expect '(find-turn2 30000000 '(8 13 1 0 18 9)) 11962)