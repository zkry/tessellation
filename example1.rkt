#lang racket
(require "tessellation.rkt")

(define g
 (generate-grid
  (circle 1)
  
  (curve (pt -1 1) .. (pt 1 -1))
  (curve (pt -1 -1) .. (pt 1 1))
  
  (curve (pt -1 -1) .. (pt -1 1))
  (curve (pt -1 1) .. (pt 1 1))
  (curve (pt 1 1) .. (pt 1 -1))
  (curve (pt -1 -1) .. (pt 1 -1))

  (rotate90 (hmirror (vmirror (curve (med -1 a q) .. q))))
  
  (curve (pt 0 -1) .. (pt 0 1))
  (curve (pt -1 0) .. (pt 1 0))))

(display-grid g)

(with-window (window -1 5 -1 5)
  (let ((apricot (make-color* #xFF #xCC #xB2))
        (yellow (make-color* #xFF #xF4 #x82))
        (cornsilk (make-color* #xFF #xF6 #xDC))
        (deep-taupe (make-color* #x7A #x62 #x63))
        (brown (make-color* #xE4 #xB7 #x47))
        (inner-star (with-grid g (curve aq -- am -- v -- ai -- ah -- al -- w -- ag -- aj -- an -- u -- ak -- ap -- ao -- x -- ar -- cycle))))
    
    (draw
     ;; The stars around the center.
     (color apricot
                 (tessellate g 3 3
                             (fill inner-star)))
     ;; The center star.
     (color yellow
            (tessellate g (1 1) (1 1)
                        (fill inner-star)))

     ;; The background.
     (color cornsilk
            (tessellate g 3 3
                        (fill (rotate90 (hmirror (vmirror (curve ab -- a -- ai -- k -- cycle)))))))

     ;; The lower, dark wire frame.
     (color deep-taupe
            (penwidth 20
                      (tessellate g 3 3
                                  (curve v -- k -- w -- q -- u -- t -- x -- f -- v)
                                  (rotate/4 (curve ab -- aq -- af)))))

     ;; Divede the lower frame into two.
     (color "white"
            (penwidth 3
                      (tessellate g 3 3
                                  (curve v -- k -- w -- q -- u -- t -- x -- f -- v)
                                  (rotate/4 (curve ab -- aq -- af)))))

     ;; The golden frame on top.
     (penwidth 6 (color brown
                        (tessellate g 3 3
                                    inner-star
                                    (vmirror (hmirror (curve h -- v)))))))))
