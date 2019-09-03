#lang scribble/manual

@(require (for-label tessellation
		  			 metapict)
		  scribble/example
          racket/sandbox
		  tessellation)
		  
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
				  (make-evaluator 'racket/base #:requires '(tessellation))))

@title{Tessellation}

@defmodule[tessellation]
Tessellation is a library for for creating tessellating geometric patterns. Instead of depending on generative algorithms, this library's approach depends on using a base-grid, then using that grid to define a tessellation pattern. @racket[set-scale]. xxx. @racket[let].

@defproc[(set-scale [size symbol?]) void?]{
Set the scale to display the images. The size is a flag indicating how big to display. Valid input is @racket['small], @racket['medium], @racket['large], @racket['x-large].
}

@defform[(generate-grid curve ...)
		  #:contracts ([curve curve?])]{
  Generates a grid from the input curves. Curves can refer to intersection points of previous curves by referring to their name. You may use to view the current state of the grid to see which points are bound.

Example of creating a simple grid:
@racketblock[(generate-grid (circle 1)
			   (curve (pt -1 -1) .. (pt 1 1))
			   (curve (pt -1 1) .. (pt 1 -1))
			   (curve a .. c))]}

@defthing[square-frame (listof shape?)]{
This is a pre-defined list of the curves of the perimiter of the square contained by the points (-1, -1) and (1, 1). This is also the tesselation bountry. This can be used directly with @racket[generate-grid].
}

@defform[(with-grid grid body ...)
		  #:contracts ([grid base-grid?]
		  			   [body any?])]{
Bind the points of @racket[grid] to variable names so that names can be referred to in @racket[body]. In total there are 702 available variable names to be bound.
}

@defform[(tessellate grid width-spec height-spec curves ...)
	      #:grammar
		  [(width-spec (code:line)
		  			   @racket[positive-integer?]
					   range-spec)
		  (height-spec (code:line)
		  			   positive-integer?
					   		  range-spec)
		  (range-spec (code:line)
		  			  (start-ct end-ct))]
		  #:contracts ([curves (listof pict?)])]

@defproc[(display-grid [grid base-grid?]) pict?]{
Takes a created grid and generates a pict. This is helpful to use when designing a layout for your design. Each intersection will be displayed as a node with a letter on it. You can use this letter as a pre-defined point inside special forms such as @racket[tessellate], @racket[with-grid], and @racket[generate-grid].
}

@deftogether[(@defproc[(vmirror [curves (listof curve?)]) (listof curve?)]
			  @defproc[(hmirror [curves (listof curve?)]) (listof curve?)])]{
@racket[vmirror] and @racket[hmirror] define functions that operate on lists of curves. For each curve passed in in the input list, two curves are returned: the original on a curve reflected across the x axis for @racket[vmirror] and the y axis for @racket[hmirror].
}

@deftogether[(@defproc[(rotate/4 [curves (listof curve?)]) (listof curve?)]
			  @defproc[(rotate/8 [curves (listof curve?)]) (listof curve?)]
			  @defproc[(rotate/16 [curves (listof curve?)]) (listof curve?)])]{
These functions return 4/8/16 curves for every curve passed into @racket[curves] with each curve rotated 90/45/22.5 degrees around the origin.
}

@deftogether[(@defproc[(rotate90 [curves (listof curve?)]) (listof curve?)]
			  @defproc[(rotate45 [curves (listof curve?)]) (listof curve?)]) ]{
Simmilar to the Xmirror and rotate/X functions, these take a list of curves and returns a list of twice the size, with every curve rotated 90/45 degrees clockwise across the origin.
}

Below is an example of what can be built with this library:

@image{scribblings/example1.png}

The code to generate this is as follows:

@racketblock[
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
]
