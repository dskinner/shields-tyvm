(define-module (dom canvas)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (call-with-restore
            current-context
            save
            restore
            fill-style
            font
            text-align
            clear-rect
            fill-rect
            fill-text
            begin-path
            fill
            arc
            stroke
            move-to
            line-to
            line-width
            stroke-style
            translate
            scale
            transform))

(include "canvas/ffi.scm")

(define current-context (make-parameter #f))

;; TODO macro to inline thunk body ???
(define (call-with-restore thunk)
  (save)
  (thunk)
  (restore))

(define (fill-style rgba) (canvas-fill-style (current-context) rgba))

(define (font v) (canvas-font (current-context) v))

(define (text-align v) (canvas-text-align (current-context) v))

(define (clear-rect x y w h) (canvas-clear-rect-n4 (current-context) x y w h))

(define (fill-rect x y w h) (canvas-fill-rect-n4 (current-context) x y w h))

(define (fill-text text x y) (canvas-fill-text-n3 (current-context) text x y))

(define (scale x y) (canvas-scale-n2 (current-context) x y))

(define (transform a b c d e f) (canvas-transform-n6 (current-context) a b c d e f))

(define (begin-path) (canvas-begin-path-n0 (current-context)))

(define (fill) (canvas-fill-n0 (current-context)))

(define (stroke) (canvas-stroke-n0 (current-context)))

(define (arc x y r start end) (canvas-arc-n5 (current-context) x y r start end))

(define (move-to x y) (canvas-move-to-n2 (current-context) x y))

(define (line-to x y) (canvas-line-to-n2 (current-context) x y))

(define (line-width w) (canvas-line-width (current-context) w))

(define (stroke-style value) (canvas-stroke-style (current-context) value))

(define (save) (canvas-save-n0 (current-context)))

(define (restore) (canvas-restore-n0 (current-context)))

(define (translate x y) (canvas-translate-n2 (current-context) x y))
