;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; HTMLCanvasElement and CanvasRenderingContext2D bindings.
;;;
;;; Code:

(define-module (dom canvas)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (get-context
            fill-color
            font
            text-align
            clear-rect
            fill-rect
            fill-text
            draw-image
            scale
            transform
            image-smoothing-enabled
            begin-path
            fill
            arc
            stroke
            move-to
            line-to
            line-width
            stroke-style
            context
            save
            restore
            translate))


(define context (make-parameter #f))

(define-foreign get-context
  "canvas" "getContext"
  (ref extern) (ref string) -> (ref extern))

(define-foreign canvas-fill-color
  "canvas" "setFillColor"
  (ref extern) (ref string) -> none)
(define (fill-color rgba) (canvas-fill-color (context) rgba))

(define-foreign canvas-font
  "canvas" "setFont"
  (ref extern) (ref string) -> none)
(define (font v) (canvas-font (context) v))

(define-foreign canvas-text-align
  "canvas" "setTextAlign"
  (ref extern) (ref string) -> none)
(define (text-align v) (canvas-text-align (context) v))

(define-foreign canvas-clear-rect
  "canvas" "clearRect"
  (ref extern) f64 f64 f64 f64 -> none)
(define (clear-rect x y w h) (canvas-clear-rect (context) x y w h))

(define-foreign canvas-fill-rect
  "canvas" "fillRect"
  (ref extern) f64 f64 f64 f64 -> none)
(define (fill-rect x y w h) (canvas-fill-rect (context) x y w h))

(define-foreign canvas-fill-text
  "canvas" "fillText"
  (ref extern) (ref string) f64 f64 -> none)
(define (fill-text text x y) (canvas-fill-text (context) text x y))

(define-foreign canvas-scale
  "canvas" "setScale"
  (ref extern) f64 f64 -> none)
(define (scale x y) (canvas-scale (context) x y))

(define-foreign canvas-transform
  "canvas" "setTransform"
  (ref extern) f64 f64 f64 f64 f64 f64 -> none)
(define (transform a b c d e f) (canvas-transform (context) a b c d e f))

(define-foreign canvas-begin-path
  "canvas" "beginPath"
  (ref extern) -> none)
(define (begin-path) (canvas-begin-path (context)))

(define-foreign canvas-fill
  "canvas" "fill"
  (ref extern) -> none)
(define (fill) (canvas-fill (context)))

(define-foreign canvas-stroke
  "canvas" "stroke"
  (ref extern) -> none)
(define (stroke) (canvas-stroke (context)))

(define-foreign canvas-arc
  "canvas" "arc"
  (ref extern) f32 f32 f32 f32 f32 -> none)
(define (arc x y r start end) (canvas-arc (context) x y r start end))

(define-foreign canvas-move-to
  "canvas" "moveTo"
  (ref extern) f32 f32 -> none)
(define (move-to x y) (canvas-move-to (context) x y))

(define-foreign canvas-line-to
  "canvas" "lineTo"
  (ref extern) f32 f32 -> none)
(define (line-to x y) (canvas-line-to (context) x y))

(define-foreign canvas-line-width
  "canvas" "lineWidth"
  (ref extern) f32 -> none)
(define (line-width w) (canvas-line-width (context) w))

(define-foreign canvas-stroke-style
  "canvas" "strokeStyle"
  (ref extern) (ref string) -> none)
(define (stroke-style value) (canvas-stroke-style (context) value))

(define-foreign canvas-save
  "canvas" "save"
  (ref extern) -> none)
(define (save) (canvas-save (context)))

(define-foreign canvas-restore
  "canvas" "restore"
  (ref extern) -> none)
(define (restore) (canvas-restore (context)))

(define-foreign canvas-translate
  "canvas" "translate"
  (ref extern) f32 f32 -> none)
(define (translate x y) (canvas-translate (context) x y))

