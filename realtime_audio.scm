(import (scheme base)
        (scheme inexact)
        (only (hoot bytevectors) bytevector-ieee-single-native-set!)
        (hoot debug)
        (hoot ffi)
        (only (hoot syntax) case-lambda define* lambda*))

(define (hertz->angular hz)
  (* twopi hz))
(define (hertz->angular/normalize hz sr)
  (/ (hertz->angular hz) sr))
(define (decibel->amp dB)
  (expt 10 (/ dB 20)))

(define twopi (* 2 (acos -1)))
(define default-sample-rate 48000) ;; set from worklet
(define default-buffer-length 128) ;; webaudio api default
;; (define default-amp-factor (decibel->amp -10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; signal.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-table-size 4096)

(define (discrete-interp sig t)
  "Uses the fractional component of t to return an interpolated sample."
  (when (< t 0)
    (set! t (- t)))
  (set! t (- t (truncate t)))
  (set! t (* t (vector-length sig)))
  (define frac (- t (truncate t)))
  (define i (exact (truncate (- t frac))))
  (if (= 0 frac)
      (vector-ref sig i)
      (let ((j (+ 1 i)))
        (when (= j (vector-length sig))
          (set! j 0))
        (+ (* (- 1 frac) (vector-ref sig i)) (* frac (vector-ref sig j))))))

(define (discrete-at sig t)
  "Uses the fractional component of t to return the sample at the truncated index."
  (when (< t 0)
    (set! t (- t)))
  (set! t (- t (truncate t)))
  (set! t (* t (vector-length sig)))
  (vector-ref sig (exact (truncate t))))

(define (discrete-index sig i)
  (vector-ref sig (modulo i (- (vector-length sig) 1))))

(define (discrete-sample sig src interval phase)
  "Reads values from src at phase by interval and returns next phase to sample."
  (let lp ((i 0))
    (if (< i (vector-length sig))
        (begin
          (vector-set! sig i (src phase))
          (set! phase (+ phase interval))
          (lp (+ 1 i)))
        phase)))

(define (continuous-sine t)
  (sin (* twopi t)))

(define (discrete-sine)
  (define sig (make-vector default-table-size))
  (discrete-sample sig continuous-sine (/ 1 default-table-size) 0)
  sig)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; oscil.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (oscil in #:key (freq 440) (mod-freq #f) (phase 0) (mod-phase #f) (amp 1) (mod-amp #f))
  (let ((out (make-vector default-buffer-length)))
    (case-lambda
      (() out)
      ((tc newfreq)
       (when newfreq
         (set! freq newfreq))
       (define frame (* (- tc 1) (vector-length out)))
       (define nfreq (/ freq default-sample-rate))
       (do ((i 0 (+ 1 i))
            (interval nfreq nfreq)
            (offset 0 0)
            (ampfac amp amp))
           ((= i (vector-length out)) out)
         (when mod-freq
           (set! interval (* interval (discrete-index (mod-freq) (+ frame i)))))
         (when mod-phase
           (set! offset (discrete-index (mod-phase) (+ frame i))))
         (when mod-amp
           (set! ampfac (* ampfac (discrete-index (mod-amp) (+ frame i)))))
         (vector-set! out i (* ampfac (discrete-at in (+ phase offset))))
         (set! phase (+ phase interval)))))))

;; TODO additive mixing ???
(define (sum l i)
  (if (null? l)
      0
      (+ (vector-ref (car l) i) (sum (cdr l) i))))

(define (mixer . ins)
  (let ((out (make-vector default-buffer-length)))
    (lambda ()
      (do ((i 0 (+ 1 i)))
          ((= i (vector-length out)) out)
        (vector-set! out i (sum ins i))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; filter.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    ;; TODO low-pass

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; delay.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    ;; TODO tap

;;    ;; TODO comb
   
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;    ;; TODO create graph

;; possible shield starting configurations as main-freq:mod-freq
;; 176:748
;; 88:754
;; 88:559.4 interesting
;; 88:374
;; 88:42
;; 100:748

;; 100:62

;; looks like shields, could overlay over sword?
;; 3857.2
;; 0.5
;; 914.7
;; 0.5
;;
;; 3857.2
;; 0.5
;; 0.5
;; 3857.2
;;

;; ---
;; 1800.3
;; 743.3
;; ---

;; 343.3
;; 29.1
;; 257.6
;; 1.9, then 0.5 or 2

;; everything 0.5hz but setup mod-amp to 400hz

;; try keep a lot of things in 0-10hz range

;; fun mixer stuff
;; ---
;; 111.8, 60.4->54.7->57.6
;; 240.2, 171.7
;; ---
;; (define ooo (mixer
;;              (oscil sine
;;                     #:freq 111.8 #:mod-freq (oscil sine 60.4)
;;                     #:amp (decibel->amp -10))
;;              (oscil sine
;;                     #:freq 240.2)))

(define sine (discrete-sine))

;; user controls
(define osc-uc1 (oscil sine))
(define osc-uc0 (oscil sine #:mod-freq osc-uc1 #:amp (decibel->amp -10)))
;; (define osc-uc1 (oscil sine))
;; (define osc-uc0 (oscil sine))

;; shield controls
(define osc-sh1 (oscil sine #:mod-freq osc-uc0))
(define osc-sh0 (oscil sine #:mod-freq osc-sh1 #:amp (decibel->amp -10)))

(define mix (mixer (osc-sh0) ))

(define time-code 0)
(define buf (make-bytevector (* 4 default-buffer-length)))

(define (prepare sh0 sh1 uc0 uc1)
  (set! time-code (+ 1 time-code))

  ;; TODO sort and prepare graph
  (osc-uc1 time-code uc1)
  (osc-uc0 time-code uc0)
  (osc-sh1 time-code sh1)
  (osc-sh0 time-code sh0)

  ;; (define out (osc-sh0))
  (define out (mix))
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length out)) buf)
    (bytevector-ieee-single-native-set! buf (* 4 i) (vector-ref out i))))
(define prepare* (procedure->external prepare))

(define (default-sample-rate-set! sr)
  (set! default-sample-rate sr))
(define default-sample-rate-set!* (procedure->external default-sample-rate-set!))
 
(values prepare* default-sample-rate-set!*)
