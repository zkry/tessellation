#lang racket
(require metapict)
(require (for-syntax racket/list
                     racket/format))

(define node-size 0.3)

(define (set-scale scale)
  (match scale
    ['small
     (set-curve-pict-size 500 500)
     (set! node-size 0.03)]
    ['medium
     (set-curve-pict-size 800 800)
     (set! node-size 0.03)]
    ['large
     (set-curve-pict-size 1200 1200)
     (set! node-size 0.015)]
    ['x-large
     (set-curve-pict-size 2400 2400)
     (set! node-size 0.01)]))

(set-scale 'large)

(struct base-grid (points shapes) #:prefab)

(struct filled-curve (curve) #:prefab)

;; How do I define the same thing (constant) for multiple levels?
(define-for-syntax pt-ids
  (let ((base-ids (map (lambda (offset) (string (integer->char (+ (char->integer #\a) offset)))) (range 26))))
    (append base-ids (for*/list ((i base-ids) (j base-ids)) (~a i j)))))
(define pt-ids
  (let ((base-ids (map (lambda (offset) (string (integer->char (+ (char->integer #\a) offset)))) (range 26))))
    (append base-ids (for*/list ((i base-ids) (j base-ids)) (~a i j)))))

(define-for-syntax (pt-id stx)
  (and (identifier? stx)
       (index-of pt-ids (symbol->string (syntax->datum stx)))))
(define (pt-id stx)
  (and (identifier? stx)
       (index-of pt-ids (symbol->string (syntax->datum stx)))))

(define (pt-equal? a b)
  (and (< (abs (- (pt-x a) (pt-x b))) 1e-5)
       (< (abs (- (pt-y a) (pt-y b))) 1e-5)))

;; points-deduplicate returns the set of points in a
;; not in b.
(define (pt-deduplicate a b)
  (filter (lambda (pa)
            (not (for/or ([pb b]) (pt-equal? pa pb))))
          a))

(define (map-bez bezs f)
  (map
   (lambda (b)
     (bez (f (bez-p0 b))
          (f (bez-p1 b))
          (f (bez-p2 b))
          (f (bez-p3 b))))
   bezs))

(define square-frame
  (list
   (curve (pt -1 -1) .. (pt -1 1))
   (curve (pt -1 1) .. (pt 1 1))
   (curve (pt 1 1) .. (pt 1 -1))
   (curve (pt -1 -1) .. (pt 1 -1))))

(define (rotate90 c)
  (define (rotate90-curve c)
    (list c ((rotatedd (- 90)) c)))
  (flatten (map rotate90-curve (flatten c))))

(define (rotate45 c)
  (define (rotate45-curve c)
    (list c ((rotatedd (- 45)) c)))
  (flatten (map rotate45-curve (flatten c))))

(define (rotate-curve-lambda angle n)
  (lambda (c)
    (map (lambda (n) ((rotatedd (* n angle)) c)) (range n))))

(define (rotate/4 c)
  (flatten (map (rotate-curve-lambda 90.0 4) (flatten c))))

(define (rotate/8 c)
  (flatten (map (rotate-curve-lambda 45.0 8) (flatten c))))

(define (rotate/16 c)
  (flatten (map (rotate-curve-lambda 22.5 16) (flatten c))))

(define (hmirror c)
  (define (pt-hflip p)
    (pt (- (pt-x p)) (pt-y p)))
  (define (hmirror-curve c)
    (defm (curve closed? bezs) c)
    (list c (curve: closed? (map-bez bezs pt-hflip))))
  (flatten (map hmirror-curve (flatten c))))

(define (vmirror c)
  (define (pt-vflip p)
    (pt (pt-x p) (- (pt-y p))))
  (define (vmirror-curve c)
    (defm (curve closed? bezs) c)
    (list c (curve: closed? (map-bez bezs pt-vflip))))
  (flatten (map vmirror-curve (flatten c))))

;; Return lambda that translates curve by x, y. Used for tessellation.
(define (translate x y)
  (define (translate-pt x y)
    (lambda (p)
      (pt (+ x (pt-x p)) (+ y (pt-y p)))))
  (lambda (c)
    (match c
      [(? filled-curve?)
       (let ((c (filled-curve-curve c)))
         (defm (curve closed? bezs) c)
         (fill (curve: closed? (map-bez bezs (translate-pt x y)))))]
      [_
       (defm (curve closed? bezs) c)
       (curve: closed? (map-bez bezs (translate-pt x y)))])))

(define (fill-wrap x)
  (map filled-curve (flatten x)))

(define-syntax (process-curve stx)
  (syntax-case stx ()
    [(_ points (f x ...))
     (if (equal? (syntax->datum #'f) 'fill)
         #'(process-curve points (fill-wrap x ...)) ; We want process-curve to remove all instances of fill, so replace fill with identity.
         #'(f (process-curve points x) ...))]
    [(_ points id)
     (if (pt-id #'id)
         #'(list-ref points (pt-id #'id))
         #'id)]))

(define-syntax (generate-grid stx)
  (syntax-case stx ()
    [(_ curve ...)
     #'(let ((points '())
             (shapes '()))
         (let* ((processed-curves (flatten (process-curve points curve)))
                (new-points (for*/fold ([pt-acc '()])
                                       ([i (append shapes processed-curves)]
                                        [j (flatten (list processed-curves))])
                              (if (equal? i j)
                                  pt-acc
                                  (let ((next-pts (pt-deduplicate (intersection-points i j) pt-acc)))
                                    (append pt-acc next-pts))))))
           (set! points (append points (pt-deduplicate new-points points)))
           (set! shapes (append shapes (flatten processed-curves)))) ...
        (base-grid points shapes))]))

(def (node p id)
  (def circ (circle p node-size))
  (def filled   (color "white" (fill circ)))
  (def label    (label-cnt (~a id) p))
  (draw filled circ label))

(define (display-grid grid)
  (draw (for/draw ([s (base-grid-shapes grid)])
          s)
        (for/draw ([pt (base-grid-points grid)]
                   [id pt-ids])
          (node pt id))))


;; TODO: DRY this macro up.
(define-syntax (tessellate stx)
  (syntax-case stx ()
    [(_ g (width-start width-end) (height-start height-end) curves ...)
     #'(draw (for/draw ([xy (for*/list ([x (range (* 2 width-start) (* 2 (add1 width-end)) 2)]
                                        [y (range (* 2 height-start) (* 2 (add1 height-end)) 2)])
                              (cons x y))])
               (for/draw ([s (map (translate (car xy) (cdr xy))
                                  (flatten (list (process-curve (base-grid-points g) curves) ...)))])
                 s)))]
    [(_ g width height curves ...)
     #'(draw (for/draw ([xy (for*/list ([x (range 0 (* 2 width) 2)]
                                        [y (range 0 (* 2 height) 2)])
                              (cons x y))])
               (for/draw ([s (map (translate (car xy) (cdr xy))
                                  (flatten (list (process-curve (base-grid-points g) curves) ...)))])
                 s)))]))

(define-syntax (with-grid stx)
  (syntax-case stx ()
    [(_ grid body ...)
     #'(let ((points (base-grid-points grid)))
         (process-curve points body) ...)]))

(provide set-scale
         with-grid
         tessellate
         display-grid
         generate-grid
         vmirror
         hmirror
         rotate/4
         rotate/8
         rotate/16
         rotate90
         rotate45
         square-frame
         
         (all-from-out metapict))
