(import (scheme base)
        (scheme inexact)
        (scheme write)
        (only (hoot bytevectors) bytevector-ieee-single-native-set! bytevector-ieee-single-native-ref)
        (hoot debug)
        (hoot hashtables)
        (hoot ffi)
        (only (hoot syntax) case-lambda define*)
        (dom canvas)
        (webaudio))

(define-foreign canvas-get-context
  "element" "getContext"
  (ref extern) (ref string) -> (ref extern))

(define-foreign get-element-by-id
  "document" "getElementById"
  (ref string) -> (ref null extern))

(define-foreign add-event-listener!
  "event" "addEventListener"
  (ref extern) (ref string) (ref extern) -> none)

(define-foreign prevent-default!
  "event" "preventDefault"
  (ref extern) -> none)

(define-foreign offset-x
  "event" "offsetX"
  (ref extern) -> i32)

(define-foreign offset-y
  "event" "offsetY"
  (ref extern) -> i32)

(define-foreign touch-x
  "event" "touchX"
  (ref extern) -> i32)

(define-foreign touch-y
  "event" "touchY"
  (ref extern) -> i32)

(define-foreign element-width
  "element" "width"
  (ref extern) -> i32)

(define-foreign element-height
  "element" "height"
  (ref extern) -> i32)

(define-foreign make-array-f32
  "array" "newFloat32Array"
  i32 -> (ref extern))

(define-foreign array-f32-ref
  "array" "getFloat32Array"
  (ref extern) i32 -> f32)

(define-foreign array-f32-set!
  "array" "setFloat32Array"
  (ref extern) i32 f32 -> none)

(define-foreign array-f32-buf
  "array" "bufFloat32Array"
  (ref extern) -> (ref extern))

(define-foreign request-animation-frame
  "window" "requestAnimationFrame"
  (ref null extern) -> (ref null extern))

(define-foreign timeout
  "window" "setTimeout"
  (ref extern) f64 -> i32)

(define-foreign get-float-time-domain-data
  "analyser" "getFloatTimeDomainData"
  -> (ref null extern))

(define-foreign get-rms
  "analyser" "getRMS"
  -> f32)

(define-foreign random
  "math" "random"
  -> f32)

(define difficulty 10.0)

(define canvas (get-element-by-id "canvas"))
(current-context (canvas-get-context canvas "2d"))
(define canvas-width (element-width canvas))
(define canvas-height (element-height canvas))

(define (unit-x ev)
  (/ (offset-x ev) canvas-width))
(define (unit-y ev)
  (- 1 (/ (offset-y ev) canvas-height)))

(define (touch-unit-x ev)
  (/ (touch-x ev) canvas-width))
(define (touch-unit-y ev)
  (- 1 (/ (touch-y ev) canvas-height)))

(define param-freq (audio-param-get "freq"))
(define param-modfreq (audio-param-get "modfreq"))
(define param-modphase (audio-param-get "modphase"))

(define (param-unit-val p)
  ;; TODO account for min
  (/ (audio-param-val p) (audio-param-max p)))

(define (trace . objs)
  (for-each display objs)
  (newline)
  (flush-output-port))

(define (trace-mouse-event ev)
  (trace "[x=" (unit-x ev) "][y=" (unit-y ev) "]"))

(define want-modphase (audio-param-val param-modphase))
(define want-modfreq (audio-param-val param-modfreq))
(define mouse-down? #f)
(define touch-down? #f)

(define (audio-context-toggle)
  (if (string=? "running" (audio-context-state))
      (audio-context-suspend)
      (audio-context-resume)))

(define (on-mouse-down ev)
  (prevent-default! ev)
  (set! mouse-down? #t)
  (handle-mouse-event ev))

(define (on-mouse-move ev)
  (prevent-default! ev)
  (when mouse-down?
    (handle-mouse-event ev)))

(define (on-mouse-up ev)
  (prevent-default! ev)
  (set! mouse-down? #f)
  (set! want-modphase 0.0))

(define (handle-mouse-event ev)
  (set! want-modphase (* (unit-y ev) (audio-param-max param-modphase)))
  (set! want-modfreq (* (unit-x ev) (audio-param-max param-modfreq))))

;; TODO should only handle first touch event
(define (on-touch-start ev)
  (prevent-default! ev)
  (set! touch-down? #t)
  (trace (touch-unit-x ev) ":" (touch-unit-y ev))
  (handle-touch-event ev))

(define (on-touch-move ev)
  (prevent-default! ev)
  (when touch-down?
    (handle-touch-event ev)))

(define (on-touch-end ev)
  (prevent-default! ev)
  (set! touch-down? #f)
  (set! want-modphase 0.0))

(define (handle-touch-event ev)
  (set! want-modphase (* (touch-unit-y ev) (audio-param-max param-modphase)))
  (set! want-modfreq (* (touch-unit-x ev) (audio-param-max param-modfreq))))

(define (lt-eps eps x y)
  (< eps (abs (- x y))))

(define (handle-wants)
  (let ((have-modphase (audio-param-val param-modphase))
        (have-modfreq (audio-param-val param-modfreq)))

    (when (or (= difficulty 10) (= difficulty 8))
      (audio-param-set! param-modphase want-modphase)
      (audio-param-set! param-modfreq want-modfreq))

    (when (= difficulty 4)
      (if (lt-eps 40.0 want-modfreq have-modfreq)
        (if (< want-modfreq have-modfreq)
            (audio-param-set! param-modfreq (- have-modfreq 40.0))
            (audio-param-set! param-modfreq (+ have-modfreq 40.0)))
        (audio-param-set! param-modfreq want-modfreq))
      
      (if (lt-eps 0.01 want-modphase have-modphase)
        (if (< want-modphase have-modphase)
            (audio-param-set! param-modphase (- have-modphase 0.01))
            (audio-param-set! param-modphase (+ have-modphase 0.01)))
        (audio-param-set! param-modphase want-modphase))
      )
    ))

(define-record-type <game>
  (make-game incoming damage attacks perfects)
  game?
  (incoming game-incoming set-game-incoming!)
  (damage game-damage set-game-damage!)
  (attacks game-attacks set-game-attacks!)
  (perfects game-perfects set-game-perfects!))

(define (draw-game-incoming game)
  (fill-style "#ffffff")
  (text-align "center")
  (font "bold 16px monospace")

  (translate 300.0 16.0)

  (fill-text "Attack in" 0.0 0.0)
  (font "bold 36px monospace")
  (fill-text (number->string* (game-incoming game)) 0.0 36.0))

(define (draw-game game)
  (call-with-restore (lambda () (draw-game-incoming game)))
    
  (fill-style "#ffffff")
  (font "bold 16px monospace")
  (text-align "left")

  (translate 484.0 16.0)

  (fill-text "Survived:" 0.0 0.0)
  (fill-text (number->string* (game-attacks game)) 88.0 0.0) 

  (translate 0.0 16.0)
  (fill-text "Perfects:" 0.0 0.0)
  (fill-text (number->string* (game-perfects game)) 88.0 0.0)

  (translate 0.0 16.0)
  (fill-text (string-append "  Damage:" (number->string* (to-fixed (game-damage game) 1)) "%") 0.0 0.0))

(define (rand-f32 s e)
  (+ s (* (random) (- e s))))

(define (set-param-random! p)
  (let ((x (rand-f32 (audio-param-min p) (audio-param-max p))))
    (audio-param-set! p x)
    x))

(define (generate-attack)
  (set-param-random! param-freq)
  (set! want-modfreq (set-param-random! param-modfreq))
  (set! want-modphase (set-param-random! param-modphase)))

(define current-game (make-game 10 0.0 0 0))

(define (game-loop)
  (when (= 0 (game-incoming current-game))
    (set-game-incoming! current-game 10)
    (set-game-attacks! current-game (+ (game-attacks current-game) 1))
    (let ((rms (get-rms)))
      (if (>= rms difficulty)
          (set-game-damage! current-game (+ (game-damage current-game) (* 1.5 rms)))
          (if (>= rms 1.0)
              (set-game-damage! current-game (+ (game-damage current-game) (/ rms 2.0)))
              (set-game-perfects! current-game (+ (game-perfects current-game) 1))
              )))
    (when (< (game-damage current-game) 100.0)
      (generate-attack)))
  (set-game-incoming! current-game (- (game-incoming current-game) 1))
  (if (< (game-damage current-game) 100.0)
    (timeout game-loop* 1000.0)
    (audio-context-suspend))
  )
(define game-loop* (procedure->external game-loop))
(timeout game-loop* 1000.0)

(define twopi (* 2 (acos -1)))

(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (to-fixed f n)
  (let ((d (expt 10 n)))
    (/ (truncate (* f d)) d)))

(define (param->string p)
  (string-append (number->string (to-fixed (audio-param-val p) 3)) "hz"))

(define (norm x) (/ (+ 1.0 x) 2.0))

(define (draw-signal-info)
  (font "bold 16px monospace")
  (text-align "left")

  (fill-style "#999")

  (translate 16.0 16.0)
  (fill-text " BASE:" 0.0 0.0)
  (fill-text (param->string param-freq) 58.0 0.0)

  (fill-style "#ffffff")

  (translate 0.0 16.0)
  (fill-text "PHASE:" 0.0 0.0)
  (fill-text (param->string param-modphase) 58.0 0.0)

  (translate 0.0 16.0)
  (fill-text " FREQ:" 0.0 0.0)
  (fill-text (param->string param-modfreq) 58.0 0.0))

(define (main-loop now)
  (handle-wants)
  (fill-style "#140c1c")
  (fill-rect 0.0 0.0 canvas-width canvas-height)

  (let ((data (get-float-time-domain-data))
        (rms (get-rms)))

    (define r (max 0.0 (min 1.0 (/ rms difficulty))))
    (define g (- 1.0 r))
    (stroke-style (string-append "rgb(" (number->string* (truncate (* 255 r))) "," (number->string* (truncate (* 255 g))) ", 0" ")"))
    
    (line-width 1.0)
    (move-to 0.0 0.0) ;; TODO
    (do ((i 0 (+ 1 i)))
        ((= i 1024) (stroke))
      (if (= 0 i)
          (move-to
           (* canvas-width (/ i 1024.0))
           (* canvas-height (norm (array-f32-ref data i))))
          (line-to
           (* canvas-width (/ i 1024.0))
           (* canvas-height (norm (array-f32-ref data i)))))))

  (fill-style "#ffffff")
  (begin-path)
  (arc (* (param-unit-val param-modfreq) canvas-width) (- canvas-height (* (param-unit-val param-modphase) canvas-height)) 5.0 0.0 twopi)
  (fill)

  (call-with-restore (lambda () (draw-signal-info)))
  (call-with-restore (lambda () (draw-game current-game)))
 
  (request-animation-frame main-loop*))
 
(define main-loop* (procedure->external main-loop))
(request-animation-frame main-loop*)

(define (difficulty-set! x)
  (set! difficulty x))
(define difficulty-set!* (procedure->external difficulty-set!))

(add-event-listener! canvas "mousemove"
                     (procedure->external on-mouse-move))
(add-event-listener! canvas "mousedown"
                     (procedure->external on-mouse-down))
(add-event-listener! canvas "mouseup"
                     (procedure->external on-mouse-up))

(add-event-listener! (get-element-by-id "toggleaudio") "click"
                     (procedure->external (lambda (ev) (audio-context-toggle))))

(add-event-listener! canvas "touchstart"
                     (procedure->external on-touch-start))
(add-event-listener! canvas "touchmove"
                     (procedure->external on-touch-move))
(add-event-listener! canvas "touchend"
                     (procedure->external on-touch-end))


(values difficulty-set!*)
