#lang racket

#|
TODO:

In at least some cases, it would probably be better to use sets than lists.

Matches can be done by relative distances to avoid rotating & translating up front.

If we merge each match into scanner zero's data we can avoid the frame-mapping mess I have.
e.g. Find a match for scanner zero.
     Merge its data into scanner zero.
     Find another match for the improved scanner zero.
     Repeat.

There may opportunities to memoize or otherwise avoid duplicating work.
|#

(require threading)
(require srfi/26)
(require "../qtest.rkt")

(define (wl msg x)
  (printf "~a: ~s\n" msg x)
  x)

(struct scanner (number beacons) #:transparent)

(struct frame-mapping (to from orientation translation) #:transparent)

(define orientation-test-data
  (with-input-from-file "orientation.txt"
    (thunk
     (define-values (os bs)
       (for/fold ((orientations '())
                  (beacons '()))
                 ((line (in-lines)))
         (cond ((string-contains? line ",")
                (define beacon
                  (~> (string-split line ",")
                      (map string->number _)))
                (values orientations
                        (cons beacon beacons)))
               ((not (non-empty-string? line))
                (values (cons beacons orientations)
                        '()))
               (else
                (values orientations beacons)))))
     (cons bs os))))

; Orientations:
; Facings: +x -x +y -y +z -z
; Up for x facings: +y -y +z -z
; Up for y facings: +x -x +z -z
; Up for z facings: +x -x +y -y

; forward, up, left
(define orientations-bad
  '((x+ y+ z-) (x+ y- z+) (x+ z+ y+) (x+ z- y-)
               (x- y+ z+) (x- y- z-) (x- z+ y-) (x- z- y+)
               (y+ x+ z+) (y+ x- z-) (y+ z+ x-) (y+ z- x+)
               (y- x+ z-) (y- x- z+) (y- z+ x+) (y- z- x-)
               (z+ x+ y-) (z+ x- y+) (z+ y+ x+) (z+ y- x-)
               (z- x+ y+) (z- x- y-) (z- y+ x-) (z- y- x+)))

(define orientations
  '((x+ y+ z+) (x+ y- z-) (x+ z+ y-) (x+ z- y+)
               (x- y+ z-) (x- y- z+) (x- z+ y+) (x- z- y-)
               (y+ x+ z-) (y+ x- z+) (y+ z+ x+) (y+ z- x-)
               (y- x+ z+) (y- x- z-) (y- z+ x-) (y- z- x+)
               (z+ x+ y+) (z+ x- y-) (z+ y+ x-) (z+ y- x+)
               (z- x+ y-) (z- x- y+) (z- y+ x+) (z- y- x-)))

(define (rotate orientation point)
  #;(printf "rotate ~s ~s\n" orientation point)
  (define (do-axis axis)
    #;(printf "do-axis ~s ~s\n" axis point)
    (case axis
      ((x+) (first point))
      ((x-) (- (first point)))
      ((y+) (second point))
      ((y-) (- (second point)))
      ((z+) (third point))
      ((z-) (- (third point)))))
  (define result (map do-axis orientation))
  #;(printf "rotate ~s ~s: ~s\n" orientation point result)
  result)

(define (point<? a b)
  #;(printf "point<? ~s ~s\n" a b)
  (match-define (list ax ay az) a)
  (match-define (list bx by bz) b)
  (cond ((ax . < . bx) #t)
        ((ax . > . bx) #f)
        ((ay . < . by) #t)
        ((ay . > . by) #f)
        ((az . < . bz) #t)
        ((az . > . bz) #f)
        (else #f)))

(define (find-orientation original rotated)
  (define one (sort original point<?))
  (define results
    (for/fold ((matches '()))
              ((orientation orientations))
      (define two
        (~> (map (cute rotate orientation <>) rotated)
            (sort _ point<?)))
      (if (equal? one two)
          (cons orientation matches)
          matches)))
  (define len (length results))
  (cond ((len . < . 1)
         (printf "NO MATCH FOUND!\n")
         #f)
        ((len . > . 1)
         (printf "MULTIPLE MATCHES FOUND!\n")
         results)
        (else (first results))))

(define (test-orientations)
  (define original (first orientation-test-data))
  (map (cute find-orientation original <>)
       (rest orientation-test-data)))

(qtest (test-orientations)
       '((y+ x+ z-) (y- x- z-) (z+ y- x+) (y- z+ x-)))

#|
for each orientation
  rotate points of scanner b
  for each point of scanner a
    for each point of scanner b
      assume these points are the same
      translate all points of scanner b
      count matches
|#

(define (rotate-points o ps)
  (map (cute rotate o <>) ps))

(define (calc-translation origin offset)
  (map - offset origin))

(define (translate-points translation points)
  (map (cute map + <> translation) points))

(define (intersect a b)
  (set->list (set-intersect (list->set a)
                            (list->set b))))

(define (point-abs=? a b)
  (equal? (map abs a)
          (map abs b)))

; scanner 1 first overlapping point (686 422 578)

; scanner 1 offset 68,-1246,-43
#|
(map + '(-618 -824 -621) '(68 -1246 -43));'(-550 -2070 -664)
(map - '(-618 -824 -621) '(68 -1246 -43));'(-686 422 -578)
(map - '(-618 -824 -621) '(-686 422 -578));'(68 -1246 -43)
(map + '(-686 422 -578) '(68 -1246 -43));'(-618 -824 -621)
(map - '(-686 422 -578) '(68 -1246 -43))
|#

; DOESN'T... Returns points of b rotated & translated into the frame of a.
; Returns frame-mapping
(define (match-scanners a b)
  ; If there are possible multiple matches, may need for/list here.
  (for/or ((o orientations))
    (define r (rotate-points o (scanner-beacons b)))
    (for*/or ((a-main (scanner-beacons a))
              (b-main r))
      (define translation (calc-translation b-main a-main))
      (when (and (equal? a-main '(-618 -824 -621))
                 (equal? b-main '(-686 422 -578)))
        (printf "A-main: ~s; b-main: ~s; translation: ~s\n"
                a-main
                b-main
                translation))
      (define t (translate-points translation r))
      (define intersection (intersect (scanner-beacons a)
                                      t))
      (cond (((length intersection) . >= . 12)
             (frame-mapping (scanner-number a)
                            (scanner-number b)
                            o
                            translation))
            (else #f)))))

(define (read-scanners file)
  (with-input-from-file file
    (thunk
     (define-values (ss sn bs)
       (for/fold ((scanners '())
                  (scanner-num #f)
                  (beacons '()))
                 ((line (in-lines)))
         (cond ((string-contains? line ",") ; parse beacon
                (define beacon
                  (~> (string-split line ",")
                      (map string->number _)))
                (values scanners
                        scanner-num
                        (cons beacon beacons)))
               ((not (non-empty-string? line)) ; end of scanner
                (values (cons (scanner scanner-num beacons)
                              scanners)
                        #f
                        '()))
               (else ; start of scanner
                (match-define (list _ num)
                  (regexp-match #px"--- scanner (\\d+) ---" line))
                (values scanners (string->number num) beacons)))))
     (reverse (cons (scanner sn bs)
                    ss)))))

; e.g. from 2 to 0
(define (find-mapping-path fmaplist to from)
  (define potential-fmaps (filter-not (λ (fmap)
                                        (or (= to (frame-mapping-from fmap))
                                            (= from (frame-mapping-to fmap))))
                                      fmaplist))
  (define from-fmaps (filter (λ (fmap)
                               (= from (frame-mapping-from fmap)))
                             potential-fmaps))
  (define answer (findf (λ (fmap)
                          (= to (frame-mapping-to fmap)))
                        from-fmaps))
  (cond (answer (list answer))
        (else
         (for/or ((fmap from-fmaps))
           (define path (find-mapping-path potential-fmaps to (frame-mapping-to fmap)))
           (if path
               (cons fmap path)
               #f)))))

(define (build-fmap-list scanners)
  (~> (cartesian-product scanners scanners)
      (filter-not (cute apply equal? <>) _)
      (map (cute apply match-scanners <>) _)
      (filter values _)))

(define (reverse-axis axis)
  (case axis
    ((x+) 'x-)
    ((x-) 'x+)
    ((y+) 'y-)
    ((y-) 'y+)
    ((z+) 'z-)
    ((z-) 'z+)))

(define (build-fmap-list-new scanners)
  (define first-half
    (~> (cartesian-product scanners scanners)
        (filter-not (cute apply equal? <>) _)
        (filter (λ (scanner-pair)
                  (apply < (map scanner-number scanner-pair)))
                _)
        (map (cute apply match-scanners <>) _)
        (filter values _)))
  (define other-half
    (map (λ (fmap)
           (frame-mapping (frame-mapping-from fmap)
                          (frame-mapping-to fmap)
                          (map reverse-axis (frame-mapping-orientation fmap))
                          (map - (frame-mapping-translation fmap))))
         first-half))
  (append first-half other-half))

; build-fmap-list-old:
#;(#(struct:frame-mapping 0 1 (x- y+ z-) (68 -1246 -43))
   #(struct:frame-mapping 1 0 (x- y+ z-) (68 1246 -43))
   #(struct:frame-mapping 1 3 (x+ y+ z+) (160 -1134 -23))
   #(struct:frame-mapping 1 4 (y+ z- x-) (88 113 -1104))
   #(struct:frame-mapping 2 4 (y+ x+ z-) (1125 -168 72))
   #(struct:frame-mapping 3 1 (x+ y+ z+) (-160 1134 23))
   #(struct:frame-mapping 4 1 (z- x+ y-) (-1104 -88 113))
   #(struct:frame-mapping 4 2 (y+ x+ z-) (168 -1125 72)))

; build-fmap-list-new:
; Looks like this doesn't work the way I thought it would
#;(#(struct:frame-mapping 0 1 (x- y+ z-) (68 -1246 -43))
   #(struct:frame-mapping 1 3 (x+ y+ z+) (160 -1134 -23))
   #(struct:frame-mapping 1 4 (y+ z- x-) (88 113 -1104))
   #(struct:frame-mapping 2 4 (y+ x+ z-) (1125 -168 72))
   #(struct:frame-mapping 1 0 (x+ y- z+) (-68 1246 43)) ; ?
   #(struct:frame-mapping 3 1 (x- y- z-) (-160 1134 23)) ; ?
   #(struct:frame-mapping 4 1 (y- z+ x+) (-88 -113 1104)) ; ?
   #(struct:frame-mapping 4 2 (y- x- z+) (-1125 168 -72))) ; ?


(define (build-fmap-hash scanners fmap-list)
  (define origin-scanner (scanner-number (first scanners)))
  (define rest-scanners (map scanner-number (rest scanners)))
  (for/hash ((i rest-scanners))
    (values i (find-mapping-path fmap-list origin-scanner i))))

(define (map-frame1 fmap beacons)
  (~> (rotate-points (frame-mapping-orientation fmap)
                     beacons)
      (translate-points (frame-mapping-translation fmap)
                        _)))
#|
for: expected a sequence for fmap, got something else:
(list* (frame-mapping 16 1 '(z+ y+ x-) '(-1338 106 -40))
       (frame-mapping 37 16 '(x+ z- y+) '(22 1179 44))
       (frame-mapping 14 37 '(y+ x+ z-) '(111 -1082 31))
       (frame-mapping 6 14 '(y+ x- z+) '(-139 106 -1087))
       (frame-mapping 19 6 '(y+ z+ x+) '(-9 -91 1335))
       (frame-mapping 17 19 '(x- z- y-) '(69 -1136 -96))
       (frame-mapping 4 17 '(x- y- z+) '(-1246 80 72))
       (frame-mapping 10 4 '(y+ z- x-) '(-128 1120 -58))
       (frame-mapping 24 10 '(y+ x- z+) '(108 -160 -1139)) #f)
|#

(define (map-frame scanner fmap-hash)
  (define fmapchain (hash-ref fmap-hash (scanner-number scanner)))
  (for/fold ((beacons (scanner-beacons scanner)))
            ((fmap fmapchain))
    (map-frame1 fmap beacons)))

(define (part1 file)
  (define scanners (read-scanners file))
  #;(printf "Scanners read\n")
  (define fmap-list (build-fmap-list scanners))
  #;(printf "Built fmap-list\n~s\n" fmap-list)
  (define fmap-hash (build-fmap-hash scanners fmap-list))
  #;(printf "Built fmap-hash\n~s\n" fmap-hash)
  (~> (cons (scanner-beacons (first scanners))
            (for/list ((scanner (rest scanners)))
              (map-frame scanner fmap-hash)))
      (apply append _)
      (list->set _)
      (set-count _)))

(define (mh-distance a b)
  (~> (map - a b)
      (map abs _)
      (apply + _)))

(qtest (mh-distance '(1105 -1205 1229) '(-92 -2380 -20)) 3621)

(define (part2 file)
  (define scanners (read-scanners file))
  (define fmap-list (build-fmap-list scanners))
  (define fmap-hash (build-fmap-hash scanners fmap-list))
  (define scanner-positions
    (cons '(0 0 0)
          (for/list ((scanner (rest scanners)))
            (define fmapchain (hash-ref fmap-hash (scanner-number scanner)))
            (first (for/fold ((pt '((0 0 0))))
                             ((fmap fmapchain))
                     (map-frame1 fmap pt))))))
  (~> (cartesian-product scanner-positions scanner-positions)
      (map (cute apply mh-distance <>) _)
      (apply max _)))

(qtest (part1 "test.txt") 79)
#;(qtest (part1 "input.txt"))
(qtest (part2 "test.txt") 3621)
#;(qtest (part2 "input.txt"))