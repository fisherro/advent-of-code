#lang racket

(require "../qtest.rkt")
(require anaphoric)

(define guesses-line
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1")
(define guesses
  (map string->number
       (string-split guesses-line ",")))

(writeln guesses)

(list->vector (string-split " 8  2 23  4 24" #rx" +"))

(define (read-file file)
  (with-input-from-file file
    (thunk
     (define picks (aand (read-line)
                         (string-split it ",")
                         (map string->number it)))
     (read-line)
     (define cards
       (let loop ((cards '()))
         (define card (for/vector ((row (in-range 0 5)))
                        (aand (read-line)
                              (string-split it #rx" +")
                              (map string->number it)
                              (list->vector it))))
         (define new-cards (cons card cards))
         (define blank (read-line))
         (if (eof-object? blank)
             new-cards
             (loop new-cards))))
     (values picks cards))))

(qtest (call-with-values (thunk (read-file "test.txt")) list))

(define (mark-all-cards cards n)
  (define (mark-one-row row)
    (vector-map (λ (cell)
                  (if (= cell n)
                      (if (= cell 0)
                          -1000
                          (- cell))
                      cell))
                row))
  (define (mark-one-card card)
    (vector-map mark-one-row card))
  (map mark-one-card cards))

(define (test-mark file)
  (define-values
    (picks cards)
    (read-file file))
  (mark-all-cards cards (first picks)))

(qtest (test-mark "test.txt"))

; The world's worst vector-ormap:
(define (vector-ormap f v)
  (ormap f (vector->list v)))

; The world's worst vector-andmap:
(define (vector-andmap f v)
  (andmap f (vector->list v)))

(define (check-for-row-wins cards)
  (define (check-row row)
    (vector-andmap negative? row))
  (define (check-one-card card)
    (vector-ormap check-row card))
  (filter check-one-card cards))

(define cards-with-no-wins
  '(#(#(14 21 17 24 4)
      #(10 16 15 9 19)
      #(18 8 23 26 20)
      #(22 11 13 6 5)
      #(2 0 12 3 7))
    #(#(3 15 0 2 22)
      #(9 18 13 17 5)
      #(19 8 7 25 23)
      #(20 11 10 24 4)
      #(14 21 16 12 6))
    #(#(22 13 17 11 0)
      #(8 2 23 4 24)
      #(21 9 14 16 7)
      #(6 10 3 18 5)
      #(1 12 20 15 19))))

(define cards-with-row-win
  '(#(#(14 21 17 24 4)
      #(10 16 15 9 19)
      #(18 8 23 26 20)
      #(22 11 13 6 5)
      #(2 0 12 3 7))
    #(#(3 15 0 2 22)
      #(9 18 13 17 5)
      #(-19 -8 -7 -25 -23)
      #(20 11 10 24 4)
      #(14 21 16 12 6))
    #(#(22 13 17 11 0)
      #(8 2 23 4 24)
      #(21 9 14 16 7)
      #(6 10 3 18 5)
      #(1 12 20 15 19))))

(define cards-with-col-win
  '(#(#(14 21 17 24 4)
      #(10 16 15 9 19)
      #(18 8 23 26 20)
      #(22 11 13 6 5)
      #(2 0 12 3 7))
    #(#(3 15 -1000 2 22)
      #(9 18 -13 17 5)
      #(19 8 -7 25 23)
      #(20 11 -10 24 4)
      #(14 21 -16 12 6))
    #(#(22 13 17 11 0)
      #(8 2 23 4 24)
      #(21 9 14 16 7)
      #(6 10 3 18 5)
      #(1 12 20 15 19))))

(qtest (check-for-row-wins cards-with-no-wins) '())
(qtest (check-for-row-wins cards-with-row-win)
       '(#(#(3 15 0 2 22)
           #(9 18 13 17 5)
           #(-19 -8 -7 -25 -23)
           #(20 11 10 24 4)
           #(14 21 16 12 6))))
(qtest (check-for-row-wins cards-with-col-win) '())

(define (check-for-col-wins cards)
  (define (ref card col row)
    (vector-ref (vector-ref card row)
                col))
  (define (check-col card col)
    (let loop ((row 0))
      (cond ((= row 5)
             #t)
            ((not (negative? (ref card col row)))
             #f)
            (else
             (loop (add1 row))))))
  (define (check-one-card card)
    (let loop ((col 0))
      (cond ((= col 5)
             #f)
            ((check-col card col)
             #t)
            (else
             (loop (add1 col))))))
  (filter check-one-card cards))

(qtest (check-for-col-wins cards-with-no-wins) '())
(qtest (check-for-col-wins cards-with-row-win) '())
(qtest (check-for-col-wins cards-with-col-win)
       '(#(#(3 15 -1000 2 22)
           #(9 18 -13 17 5)
           #(19 8 -7 25 23)
           #(20 11 -10 24 4)
           #(14 21 -16 12 6))))

(define (null->false lst)
  (if (null? lst)
      #f
      lst))

(define (check-for-win cards)
  (or (null->false (check-for-row-wins cards))
      (null->false (check-for-col-wins cards))))

(qtest (check-for-win cards-with-no-wins) #f)
(qtest (check-for-win cards-with-row-win)
       '(#(#(3 15 0 2 22)
           #(9 18 13 17 5)
           #(-19 -8 -7 -25 -23)
           #(20 11 10 24 4)
           #(14 21 16 12 6))))
(qtest (check-for-win cards-with-col-win)
       '(#(#(3 15 -1000 2 22)
           #(9 18 -13 17 5)
           #(19 8 -7 25 23)
           #(20 11 -10 24 4)
           #(14 21 -16 12 6))))

(define (calculate-score pick winner)
  (define (sum-row row)
    (aand (vector-map (λ (n)
                        (if (negative? n)
                            0
                            n))
                      row)
          (vector->list it)
          (apply + it)))
  (* pick (apply + (vector->list (vector-map sum-row winner)))))

(define (play-until-win picks cards)
  (let loop ((cards cards)
             (picks picks))
    (cond ((null? picks)
           #f)
          (else
           (define pick (first picks))
           (define marked (mark-all-cards cards pick))
           (define winner (check-for-win marked))
           (cond (winner
                  (values pick
                          (first winner)
                          (remove (first winner) marked)))
                 (else
                  (loop marked (rest picks))))))))

(define (play file)
  (define-values
    (picks cards)
    (read-file file))
  (define-values
    (pick winner losers)
    (play-until-win picks cards))
  (calculate-score pick winner))

(qtest (play "test.txt") 4512)
(qtest (play "input.txt"))

#;(member 3 '(1 2 3 4 5) (λ (a b) (printf "a: ~a; b: ~a\n" a b) (= a b)))

#;(define (match-cards marked unmarked)
    (define (match-row unmarked-row marked-row)
      (let loop ((col 0))
        (if (= col 5)
            #t
            (if (negative? (vector-ref marked-row col))
                (loop (add1 col))
                (if (= (vector-ref marked-row col)
                       (vector-ref unmarked-row col))
                    (loop (add1 col))
                    #f)))))
    (andmap (vector->list (vector-map match-row unmarked marked))))

(define (play-until-last file)
  (define-values
    (picks cards)
    (read-file file))
  (let loop ((cards cards)
             (picks picks))
    #;(printf "picks ~s\ncards ~a\n" picks (length cards))
    (define-values
      (winning-pick winning-card losers)
      (play-until-win picks cards))
    (cond ((null? losers)
           (calculate-score winning-pick winning-card))
          (else
           (define remaining-picks (member winning-pick picks))
           (loop losers remaining-picks)))))

(qtest (play-until-last "test.txt") 1924)
(qtest (play-until-last "input.txt"))

; Lots of ugliness up there.
; It probably would've been better to not start with vectors.
; Using negatives to mark spaces was maybe not a good idea?
; My Scheme/Racket gets really sloppy without refactoring.
; Might refactor later...