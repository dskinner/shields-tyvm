modules = \
  modules/dom/canvas.scm \
  modules/dom/document.scm \
  modules/dom/element.scm \
  modules/dom/event.scm \
  modules/dom/image.scm \
  modules/dom/media.scm \
  modules/dom/window.scm \
  modules/math.scm \
  modules/math/rect.scm \
  modules/math/vector.scm \
  modules/snd.scm \
  modules/webaudio.scm

realtime_audio.wasm: realtime_audio.scm $(modules)
	guild compile-wasm -L modules realtime_audio.scm -o realtime_audio.wasm

realtime_webgl.wasm: realtime_webgl.scm $(modules)
	guild compile-wasm -L modules realtime_webgl.scm -o realtime_webgl.wasm

realtime.wasm: realtime_audio.wasm realtime_webgl.wasm

game.wasm: game.scm $(modules)
	guild compile-wasm -L modules -o $@ $<

serve: game.wasm
	guile -c '((@ (hoot web-server) serve))'

bundle: realtime.wasm
	rm shields-tyvm.zip || true
	zip shields-tyvm.zip -r assets/ js-runtime/ realtime.js realtime_worklet.js realtime.css index.html realtime_audio.wasm realtime_webgl.wasm

# zip game.zip -r assets/ js-runtime/ game.js game.css game.wasm index.html

clean:
	rm -f game.wasm game.zip
