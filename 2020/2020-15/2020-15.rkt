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

(expect '(find-next '(0 3 6)) 0)
(expect '(find-next '(0 3 6 0)) 3)
(expect '(find-next '(0 3 6 0 3)) 3)
(expect '(find-next '(0 3 6 0 3 3)) 1)
(expect '(find-next '(0 3 6 0 3 3 1)) 0)
(expect '(find-next '(0 3 6 0 3 3 1 0)) 4)
(expect '(find-next '(0 3 6 0 3 3 1 0 4)) 0)

; Refactor...
; Don't make find-next reverse it each time?
; There's got to be a better append.
(define (find-turn last-turn seq-start)
  (let loop ((seq seq-start)
             (turn (add1 (length seq-start))))
    (let ((next (find-next seq)))
      #;(printf "Turn ~a: ~s\n" turn seq)
      (if (= turn last-turn)
          next
          (loop (append seq (list next))
                (add1 turn))))))

(expect '(find-turn 4 '(0 3 6)) 0)
(expect '(find-turn 5 '(0 3 6)) 3)
(expect '(find-turn 6 '(0 3 6)) 3)
(expect '(find-turn 7 '(0 3 6)) 1)
(expect '(find-turn 8 '(0 3 6)) 0)
(expect '(find-turn 9 '(0 3 6)) 4)
(expect '(find-turn 10 '(0 3 6)) 0)

(expect '(find-turn 2020 '(0 3 6)) 436)

(expect '(find-turn 2020 '(1 3 2)) 1)
(expect '(find-turn 2020 '(2 1 3)) 10)
(expect '(find-turn 2020 '(1 2 3)) 27)
(expect '(find-turn 2020 '(2 3 1)) 78)
(expect '(find-turn 2020 '(3 2 1)) 438)
(expect '(find-turn 2020 '(3 1 2)) 1836)

(expect '(find-turn 2020 '(8 13 1 0 18 9)) 755)

#;(expect '(find-turn 30000000 '(0 3 6)) 175594)
