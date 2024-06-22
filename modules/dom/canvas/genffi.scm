#!/bin/sh
# -*- Scheme -*-
exec guile --r7rs "$0"
!#

(use-modules (ice-9 rdelim)
             (ice-9 regex)
             (srfi srfi-1))

(define (hyphenate s)
  (string-trim
   (regexp-substitute/global #f "[A-Z]" s
                             'pre "-" (λ (m) (string-downcase (match:substring m))) 'post)
   #\-))

(define (string-contains-any s l)
  (fold (λ (a b) (or a b)) #f (map (λ (x) (string-contains s x)) l)))

(define (map-type s)
  (cond
   ((string= s "double") "f32")
   ((string= s "double>") "(ref extern)") ;; TODO sequence
   ((string= s "DOMPointInit)>)") "(ref extern)") ;; TODO sequence
   ((string= s "long") "i32")
   ((string= s "boolean") "i32")
   ((string= s "undefined") "none")
   ((string-contains-any s '("DOMString" "CanvasTextAlign" "CanvasTextBaseline"
                             "CanvasDirection" "CanvasFontKerning" "CanvasFontStretch"
                             "CanvasFontVariantCaps" "CanvasTextRendering"))
    "(ref string)")
   ((string-contains-any s '("ImageSmoothingQuality" "CanvasLineCap" "CanvasLineJoin"
                              "DOMMatrix2DInit" "CanvasFillRule" "Path2D" "Element"
                              "CanvasImageSource" "ImageDataSettings" "ImageData" "CanvasGradient"
                             "TextMetrics"))
    "(ref extern)")
   (else (error (format #f "unhandled type: ~a" s)))))

(define (or-type? s)
  (string-contains s " or "))

(define (anon-type s)
  (if (or-type? s) s
      (first (reverse (string-split s #\space)))))

(define (arg-optional? s)
  (string-contains s "optional"))

(define (arg-type s)
  (second (reverse (string-split s #\space))))

(define (arg-clean s)
  (let ((idx (string-rindex s #\=)))
    (if idx
        (substring s 0 idx)
        s)))

(define (parse-args s)
  (if (string-null? s)
      '()
      (let* ((args (filter (negate string-null?) (map string-trim-both (string-split s #\,))))
             (reqs (filter (negate arg-optional?) args))
             (typs (map (compose map-type arg-type string-trim-both arg-clean) reqs)))
        typs)))

(define src.scm (open-output-string))
(define src.js (open-output-string))

(define (make-name name args)
  (format #f "~aN~a" name (length args)))

(define (lex-interface-proc pre)
  (set! pre "canvas") ;; TODO take from base of mixins
  (format src.js "map[\"~a\"] = map[\"~a\"] || {};\n" pre pre)
  (λ (line)
    (cond
     ((string-match "^\\s*attribute (.*) (.*);(.*)?$" line) =>
      (λ (m)
        (let* ((name (match:substring m 2))
               (type (map-type (anon-type (match:substring m 1)))))
          (format src.js "map[\"~a\"][\"~a\"] = (obj, arg) => obj.~a = arg;\n" pre name name)
          (format src.scm "~a\n"
                  `(define-foreign ,(string-append (hyphenate pre) "-" (hyphenate name))
                     ;; ',pre ',name
                     ,(format #f "\"~a\" \"~a\"" pre name)
                     (ref extern)
                     ,type
                     -> none)))))
     ((string-match "^\\s*(\\w+) (\\w+)\\((.*)\\);" line) =>
      (λ (m)
        (let* ((name (match:substring m 2))
               (args (parse-args (match:substring m 3)))
               (uniq (make-name name args))
               (rets (match:substring m 1)))
          (format src.js "map[\"~a\"][\"~a\"] = ~a;\n" pre uniq (make-js-fn name args))
          (format src.scm "~a\n"
                  `(define-foreign ,(string-append (hyphenate pre) "-" (hyphenate uniq))
                     ,(format #f "\"~a\" \"~a\"" pre uniq)
                     (ref extern) ;; canvas element
                     ,@args
                     -> ,(map-type (anon-type rets)))))))
     ((string-match "^\\};$" line) =>
      (λ (m) (set! lexer-state lex-line)))
     ((eof-object? line) (set! lexer-state #f)))))

(define (lex-line line)
  (cond
   ((string-match "^interface mixin (\\w*) \\{$" line) =>
    (λ (m)
      (set! lexer-state (lex-interface-proc (match:substring m 1)))))
   ((eof-object? line) (set! lexer-state #f))))

(define lexer-state lex-line)

(define (fmt-js-args l)
  (let ((args (unfold (λ (x) (>= x (length l)))
                      (λ (x) (format #f "arg~a," x))
                      (λ (x) (+ 1 x))
                      0)))
    (string-trim-right (apply string-append args) #\,)))
  
(define (make-js-fn name args)
  (let* ((fmt (fmt-js-args args))
         (obj (if (= 0 (length args)) "obj" "obj,")))
    (format #f "(~a~a) => obj.~a(~a)" obj fmt name fmt)))

(define idl (open-input-file "CanvasRenderingContext2D.idl"))

(format src.js "export const map = {};\n")

;; (format src.scm "~a\n\n" '(define-module (dom canvas ffi)
;;                             #:pure
;;                             #:use-module (scheme base)
;;                             #:use-module (hoot ffi)))

(let reader ((line (read-line idl)))
  (unless (eof-object? line)
    (lexer-state line)
    (reader (read-line idl))))

(with-output-to-file "ffi.scm" (λ () (display (get-output-string src.scm))))
(with-output-to-file "ffi.js" (λ () (display (get-output-string src.js))))

(close-port idl)
(close-output-port src.scm)
(close-output-port src.js)
