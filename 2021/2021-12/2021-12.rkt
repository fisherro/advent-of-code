#lang racket

(require srfi/26)
(require threading)
(require "../qtest.rkt")

(define (read-map file)
  (with-input-from-file file
    (thunk
     (for/fold ((map (hash)))
               ((line (in-lines)))
       (match-define (list _ a b)
         (regexp-match #px"^([a-zA-Z]+)-([a-zA-Z]+)$" line))
       (~> (hash-update map a (cute cons b <>) '())
           (hash-update b (cute cons a <>) '()))))))

(read-map "test1.txt")

(define (small-cave? name)
  (char-lower-case? (string-ref name 0)))

(define (find-paths map
                    (incoming-paths '())
                    (current-room "start")
                    (path-start '()))
  (define current-path (cons current-room path-start))
  (cond ((string=? current-room "end")
         (cons current-path incoming-paths))
        (else
         (for/fold ((paths incoming-paths))
                   ((exit (hash-ref map current-room)))
           (cond ((and (small-cave? exit)
                       (member exit current-path))
                  paths)
                 (else
                  (find-paths map paths exit current-path)))))))

(define (print-paths paths)
  (~> paths
      (map (λ~> reverse
                (add-between _ ",")
                (apply string-append _))
           _)
      (sort _ string<?)
      (for-each displayln _)))

#|
(print-paths (find-paths (read-map "test1.txt")))
(newline)
(print-paths (find-paths (read-map "test2.txt")))
(newline)
|#

(define (part1 file)
  (~> (read-map file)
      (find-paths _)
      (length _)))

(qtest (part1 "test1.txt") 10)
(qtest (part1 "test2.txt") 19)
(qtest (part1 "test3.txt") 226)
(qtest (part1 "input.txt"))

(define (wl x)
  (writeln x)
  x)

(define (validate-path path)
  #;(writeln path)
  (cond (((count (cute string=? "start" <>) path) . > . 1) #f)
        (((count (cute string=? "end" <>) path) . > . 1) #f)
        (else
         (define smalls-counts (~> (filter small-cave? path)
                                   (group-by values _)
                                   (map length _)))
         (and (andmap (cute < <> 3) smalls-counts)
              ((count (cute > <> 1) smalls-counts) . < . 2)))))

#;(qtest (validate-path '("start" "A" "b" "A" "b" "A" "c" "A" "end")) #t)

(define (find-paths2 map
                     (incoming-paths '())
                     (current-room "start")
                     (path-start '()))
  (define current-path (cons current-room path-start))
  (cond ((string=? current-room "end")
         (cons current-path incoming-paths))
        (else
         (for/fold ((paths incoming-paths))
                   ((exit (hash-ref map current-room)))
           (cond ((validate-path (cons exit current-path))
                  (find-paths2 map paths exit current-path))
                 (else
                  paths))))))

(define (part2 file)
  (~> (read-map file)
      (find-paths2 _)
      #;(wl _)
      (length _)))

(qtest (part2 "test1.txt") 36)
(qtest (part2 "test2.txt") 103)
(qtest (part2 "test3.txt") 3509)
(qtest (part2 "input.txt"))