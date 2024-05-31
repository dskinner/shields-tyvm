(import (except (scheme base) bytevector bytevector-append bytevector-copy bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector? make-bytevector)
        (scheme inexact)
        (scheme write)
        (hoot bytevectors)
        ;; (only (hoot bytevectors) bytevector-ieee-single-native-set! bytevector-ieee-single-native-ref)
        (hoot debug)
        (hoot ffi)
        (only (hoot syntax) case-lambda define*))

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

(define-foreign get-memory
  "mem" "getMemory"
  -> (ref extern))

(define GL_POINTS 0)
(define GL_LINES 1)
(define GL_FLOAT 5126)
(define GL_COLOR_BUFFER_BIT 16384)
(define GL_ARRAY_BUFFER 34962)
(define GL_STATIC_DRAW 35044)
(define GL_FRAGMENT_SHADER 35632)
(define GL_VERTEX_SHADER 35633)
(define GL_COMPILE_STATUS 35713)
 
(define-foreign gl-clear-color
  "webgl" "clearColor"
  f32 f32 f32 f32 -> (ref null extern))
 
(define-foreign gl-clear
  "webgl" "clear"
  i32 -> (ref null extern))

(define-foreign gl-create-shader
  "webgl" "createShader"
  i32 -> (ref null extern))

(define-foreign gl-shader-source
  "webgl" "shaderSource"
  (ref null extern) (ref string) -> none)

(define-foreign gl-compile-shader
  "webgl" "compileShader"
  (ref null extern) -> none)

(define-foreign gl-get-shader-parameter
  "webgl" "getShaderParameter"
  (ref null extern) i32 -> i32)

(define-foreign gl-create-program
  "webgl" "createProgram"
  -> (ref null extern))

(define-foreign gl-attach-shader
  "webgl" "attachShader"
  (ref null extern) (ref null extern) -> none)

(define-foreign gl-link-program
  "webgl" "linkProgram"
  (ref null extern) -> none)

(define-foreign gl-get-attrib-location
  "webgl" "getAttribLocation"
  (ref null extern) (ref string) -> i32)

(define-foreign gl-vertex-attrib-pointer
  "webgl" "vertexAttribPointer"
  i32 i32 i32 i32 i32 i32 -> none)

(define-foreign gl-enable-vertex-attrib-array
  "webgl" "enableVertexAttribArray"
  i32 -> none)
 
(define-foreign gl-create-buffer
  "webgl" "createBuffer"
  -> (ref null extern))

(define-foreign gl-bind-buffer
  "webgl" "bindBuffer"
  i32 (ref null extern) -> none)

(define-foreign gl-buffer-data
  "webgl" "bufferData"
  i32 (ref null extern) i32 -> none)

(define-foreign gl-use-program
  "webgl" "useProgram"
  (ref null extern) -> none)

(define-foreign gl-draw-arrays
  "webgl" "drawArrays"
  i32 i32 i32 -> none)

(define-foreign request-animation-frame
  "window" "requestAnimationFrame"
  (ref null extern) -> (ref null extern))

(define-foreign get-float-time-domain-data
  "analyser" "getFloatTimeDomainData"
  -> (ref null extern))

(define-foreign get-rms
  "analyser" "getRMS"
  -> f32)

(define vsrc
  (string-append "attribute vec4 aVertexPosition;"
                 "void main() {"
                 "  gl_Position = aVertexPosition;"
                 "  gl_PointSize = 3.0;"
                 "}"))

(define fsrc
  (string-append "void main() {"
                 "  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
                 "}"))

(define (load-shader type src)
  (define shader (gl-create-shader type))
  (gl-shader-source shader src)
  (gl-compile-shader shader)
  ;; (display (gl-get-shader-parameter shader GL_COMPILE_STATUS))
  shader)

(define (init-shaders)
  (define vertex (load-shader GL_VERTEX_SHADER vsrc))
  (define fragment (load-shader GL_FRAGMENT_SHADER fsrc))
  (define program (gl-create-program))
  (gl-attach-shader program vertex)
  (gl-attach-shader program fragment)
  (gl-link-program program)
  program)

(define program (init-shaders))
(define vertex-position (gl-get-attrib-location program "aVertexPosition"))
(define buffer (gl-create-buffer))

;; https://developer.mozilla.org/en-US/docs/WebAssembly/JavaScript_interface/Memory
;; https://github.com/WebAssembly/design/issues/1231

(define buf2 (make-bytevector (* 4 2048)))
(define buf (make-array-f32 2048))
(define bufv (array-f32-buf buf))
(define mem (get-memory))
(define val -1.0)
(do ((i 0 (+ 1 i)))
    ((= i 2048) buf)
  (array-f32-set! buf i val)
  (bytevector-ieee-single-native-set! buf2 (* 4 i) val)
  (set! val (+ val 0.01)))
;; (define input-x 0.0)
;; (define input-y 0.0)
;; (define input-buffer (gl-create-buffer))

(define difficulty 8.0) ;; 7 hard, 8 medium, 9 easy
 
(define (draw now)
  
  (let ((data (get-float-time-domain-data))
        (rms (get-rms)))
    ;; values over 8 would be considered shields down, so full red
    (define r (max 0.0 (min 1.0 (/ rms difficulty))))
    (define g (- 1.0 r))
    (gl-clear-color r g 0.0 1.0)
    (gl-clear GL_COLOR_BUFFER_BIT)

    (gl-bind-buffer GL_ARRAY_BUFFER buffer)
    (gl-buffer-data GL_ARRAY_BUFFER data GL_STATIC_DRAW)

    (gl-vertex-attrib-pointer vertex-position 2 GL_FLOAT 0 0 0)
    (gl-enable-vertex-attrib-array vertex-position)

    (gl-use-program program)
    ;; TODO fix line wrapping back onto itself with draw index
    (gl-draw-arrays GL_POINTS 0 1024))
  
  (request-animation-frame draw*))
 
(define draw* (procedure->external draw))
(request-animation-frame draw*)

(define (difficulty-set! x)
  (set! difficulty x))
(define difficulty-set!* (procedure->external difficulty-set!))


;; ---------------------------------

;; TODO push events to gesture filter

(define canvas (get-element-by-id "glcanvas"))
(define canvas-w (element-width canvas))
(define canvas-h (element-height canvas))

(define (trace . objs)
  (for-each display objs)
  (newline)
  (flush-output-port))

(define (trace-mouse-event ev)
  (trace "[x=" (offset-x ev) "][y=" (offset-y ev) "]"))

(define (on-mouse-down ev)
  (prevent-default! ev)
  (trace-mouse-event ev))

(define (on-mouse-move ev)
  (prevent-default! ev)
  (trace-mouse-event ev))

(define (on-mouse-up ev)
  (prevent-default! ev)
  (trace-mouse-event ev))

(add-event-listener! canvas "mousemove"
                     (procedure->external on-mouse-move))
(add-event-listener! canvas "mousedown"
                     (procedure->external on-mouse-down))
(add-event-listener! canvas "mouseup"
                     (procedure->external on-mouse-up))

;; ;; ---------------------------------
(values draw* difficulty-set!*)
