(import (scheme base)
        (scheme inexact)
        (only (hoot bytevectors) bytevector-ieee-single-native-set!)
        (hoot debug)
        (hoot ffi)
        (only (hoot syntax) define*)
        (only (hoot syntax) case-lambda))


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

(define vsrc
  (string-append "attribute vec4 aVertexPosition;"
                 "void main() {"
                 "  gl_Position = aVertexPosition;"
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

(define red 0.01)
 ;; TODO see issues#167 variable reference of - and / result in RuntimeError: bad cast

(define op 'add)

(define buffer (gl-create-buffer))
 
(define (draw now)
  (when (> red 1)
    (set! op 'sub))
  (when (< red 0)
    (set! op 'add))
  ;; (set! red (op red 0.01))
  (if (equal? op 'add)
      (set! red (+ red 0.01))
      (set! red (- red 0.01)))
  
  (gl-clear-color red 0.5 0.0 1.0)
  (gl-clear GL_COLOR_BUFFER_BIT)

  (let ((data (get-float-time-domain-data)))
    (gl-bind-buffer GL_ARRAY_BUFFER buffer)
    (gl-buffer-data GL_ARRAY_BUFFER data GL_STATIC_DRAW)

    (gl-vertex-attrib-pointer vertex-position 2 GL_FLOAT 0 0 0)
    (gl-enable-vertex-attrib-array vertex-position)

    (gl-use-program program)
    (gl-draw-arrays GL_LINES 0 1024))
  
  (request-animation-frame draw*))
 
(define draw* (procedure->external draw))
(request-animation-frame draw*)
 
(values draw*)

;; (call-with-output-file "realtime_webgl.wasm"
;;   (λ (port)
;;     (put-bytevector port (assemble-wasm (compile src)))))
