(define-module (snd)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  #:export (discrete-sine oscil))

(define (hertz->angular hz)
  (* twopi hz))

(define (hertz->angular/normalize hz sr)
  (/ (hertz->angular hz) sr))

(define (decibel->amp dB)
  (expt 10 (/ dB 20)))

(define twopi (* 2 (acos -1)))
(define default-sample-rate 48000)
(define default-buffer-length 128) ;; web audio api default
(define default-amp-factor (decibel->amp -10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; signal.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-table-size 4096)

(define (discrete-interp sig t)
  "Uses the fractional component of t to return an interpolated sample."
  (when (< t 0)
    (set! t (- t)))
  (set! t (- t (floor t))) ;; TODO use truncate, but missing; floor works since we abs above
  (set! t (* t (vector-length sig)))
  (define frac (- t (floor t)))
  (define i (exact (floor (- t frac))))
  (if (= 0 frac)
      (vector-ref sig i)
      (let ((j (1+ i)))
        (when (= j (vector-length sig))
          (set! j 0))
        (+ (* (- 1 frac) (vector-ref sig i)) (* frac (vector-ref sig j))))))

(define (discrete-at sig t)
  "Uses the fractional component of t to return the sample at the truncated index."
  (when (< t 0)
    (set! t (- t)))
  (set! t (- t (floor t)))
  (set! t (* t (vector-length sig)))
  (vector-ref sig (exact (floor t))))

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

;; TODO optional kwargs not-implemented; freq-mod amp-mod phase-mod

;; (define* (oscil in freq . mods)
;;   (define freq-mod (if (> (length mods) 0) (car mods) #f))
;;   (let ((phase 0)
;;         (out (make-vector default-buffer-length)))
;;     (case-lambda
;;       (() out)
;;       ((tc)
;;        (define frame (* tc (vector-length out)))
;;        (define nfreq (/ freq default-sample-rate))
;;        (do ((i 0 (+ 1 i))
;;             (interval nfreq nfreq))
;;            ((= i (vector-length out)) out)
;;          (when freq-mod
;;            (set! interval (* interval (discrete-index (freq-mod) (+ frame i)))))
;;          (vector-set! out i (discrete-at in phase))
;;          (set! phase (+ phase interval)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; filter.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO low-pass

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; delay.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO tap

;; TODO comb
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main.scm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
