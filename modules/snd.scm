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
(define default-sample-rate 0) ;; set from worklet
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mixer.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum l i)
  (if (null? l)
      0
      (+ (vector-ref (car l) i) (sum (cdr l) i))))

;; additive mixer
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
 
