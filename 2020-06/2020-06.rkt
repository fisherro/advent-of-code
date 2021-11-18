#lang racket

(require srfi/26)

(define (string-empty? s)
  (= 0 (string-length s)))

(define test-data
  "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb")

(define (parse-input)
  (for/fold ((results '())
             (current '())
             #:result (if (empty? current)
                          results
                          (cons current results)))
            ((line (in-lines)))
    (if (string-empty? line)
        (if (empty? current)
            (values results current)
            (values (cons current results)
                    '()))
        (values results (append current (string->list line))))))

(define (count-votes votes)
  (apply + (map (compose1 length remove-duplicates)
                votes)))

;; Expect 11
(with-input-from-string test-data
  (thunk (count-votes (parse-input))))

;; Expect 6714
(with-input-from-file "input.txt"
  (thunk (count-votes (parse-input))))

;; Could refactor the two parse functions
;; (parse-input add-to-current add-to-results)
;; (parse-input (λ (line current) (cons line current)
;;              (λ (current results) (cons current results)))
(define (parse-input2)
  (for/fold ((results '())
             (current '())
             #:result (if (empty? current)
                          results
                          (cons current results)))
            ((line (in-lines)))
    (if (string-empty? line)
        (if (empty? current)
            (values results current)
            (values (cons current results)
                    '()))
        (values results (cons (list->set (string->list line))
                              current)))))

;; Expects 6
(with-input-from-string test-data
  (thunk (apply + (map (compose1 set-count
                                 (cute apply set-intersect <>))
                       (parse-input2)))))

;; Expects 3435
(with-input-from-file "input.txt"
  (thunk (apply + (map (compose1 set-count
                                 (cute apply set-intersect <>))
                       (parse-input2)))))