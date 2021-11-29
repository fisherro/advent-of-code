#lang racket

(define test-data '(1721
                    979
                    366
                    299
                    675
                    1456))

; Finds it multiple times
; Oh, boy...this is getting uglier & uglier...
(define (find-triple-sum data)
  (filter (negate null?)
          (for*/list ((a data)
                      (b (rest data))
                      (c (rest (rest data))))
            (if (or (= a b)
                    (= a c)
                    (= b c))
                '()
                (if (= 2020 (+ a b c))
                    (begin
                      (printf "~a + ~a + ~a = 2020\n" a b c)
                      (list a b c))
                    '())))))

(writeln (apply * (first (find-triple-sum test-data))))

(writeln
 (apply *
        (first
         (find-triple-sum
          (map string->number
               (with-input-from-file "2020-1-input.txt"
                 (thunk
                  (sequence->list (in-lines)))))))))