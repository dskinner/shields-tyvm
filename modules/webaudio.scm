(define-module (webaudio)
  #:pure
  #:use-module (scheme base)
  #:use-module (hoot ffi)
  ;; #:use-module ((hoot syntax) #:select (case-lambda define*))
  #:export (audio-param-get
            audio-param-set!
            audio-param-min
            audio-param-max
            audio-param-val
            audio-context-state
            audio-context-suspend
            audio-context-resume))

(define-foreign audio-param-get
  "webaudio" "audioParamGet"
  (ref string) -> (ref extern))

(define-foreign audio-param-set!
  "webaudio" "audioParamSet"
  (ref extern) f32 -> none)

(define-foreign audio-param-min
  "webaudio" "audioParamMin"
  (ref extern) -> f32)

(define-foreign audio-param-max
  "webaudio" "audioParamMax"
  (ref extern) -> f32)

(define-foreign audio-param-val
  "webaudio" "audioParamVal"
  (ref extern) -> f32)

(define-foreign audio-context-state
  "webaudio" "audioContextState"
  -> (ref string))

(define-foreign audio-context-suspend
  "webaudio" "audioContextSuspend"
  -> none)

(define-foreign audio-context-resume
  "webaudio" "audioContextResume"
  -> none)

;; (define (get-audio-param name)
;;   (let ((param (audio-param-get name)))
;;     (case-lambda
;;       (() (audio-param-val param))
;;       ((val) (audio-param-set! param val)))))
