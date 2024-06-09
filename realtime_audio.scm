(import (scheme base)
        (only (hoot bytevectors) bytevector-ieee-single-native-set!)
        (hoot ffi)
        (snd))

;; below is oscillator with a frequency modulator; this signal is then duplicated; the duplicate signal has a phase modulator;
;; these are placed into additive mixer; signals can cancel each other out when duplicate has phase offset.
;;
;; basically, this is a noise-cancelling game.
;;
;; the frequency modulator is mostly a source of artificial complexity; this with choice of game controls can vary difficulty
;; from nearly-impossible to trivial.

(define sine (discrete-sine))

(define osc-mod-phase (oscil sine))
(define osc-mod-freq (oscil sine))
(define osc-shield-0 (oscil sine #:mod-freq osc-mod-freq #:amp (decibel->amp -10)))
(define osc-shield-1 (oscil sine #:mod-freq osc-mod-freq #:amp (decibel->amp -10) #:mod-phase osc-mod-phase))
(define mix (mixer (osc-shield-0) (osc-shield-1)))

(define time-code 0)
(define buf (make-bytevector (* 4 default-buffer-length)))

(define (prepare freq mod-freq mod-phase)
  (set! time-code (+ 1 time-code))

  ;; TODO sort and prepare graph
  (osc-mod-phase time-code mod-phase)
  (osc-mod-freq time-code mod-freq)
  (osc-shield-0 time-code freq)
  (osc-shield-1 time-code freq)

  (define out (mix))
  (do ((i 0 (+ 1 i)))
      ((= i (vector-length out)) buf)
    (bytevector-ieee-single-native-set! buf (* 4 i) (vector-ref out i))))

(values (procedure->external prepare) (procedure->external set-default-sample-rate!))

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

;; 400:375
;; 400:1000

;; 440: 572
;; 440: 550
;; 220: 550 sounds like a warning
