let audioContext;
let audioSink;
let modules = {};

async function gameInit() {
	const fftSize = 1024;
	audioContext = new AudioContext({sampleRate: 8000});
	const analyser = audioContext.createAnalyser();
	analyser.fftSize = fftSize;
	const analyserData = new Float32Array(fftSize);

	// modules["js-runtime/wtf8.wasm"] = await WebAssembly.compileStreaming(fetch("js-runtime/wtf8.wasm"));
	// modules["js-runtime/reflect.wasm"] = await WebAssembly.compileStreaming(fetch("js-runtime/reflect.wasm"));
	// modules["realtime_audio.wasm"] = await WebAssembly.compileStreaming(fetch("realtime_audio.wasm"));
	// BUG linux/chromium failed to serialize wasm module on postMessage; instead, post ArrayBuffer and compile in worklet.
	modules["js-runtime/wtf8.wasm"] = await (await fetch("js-runtime/wtf8.wasm")).arrayBuffer();
	modules["js-runtime/reflect.wasm"] = await (await fetch("js-runtime/reflect.wasm")).arrayBuffer();
	modules["realtime_audio.wasm"] = await (await fetch("realtime_audio.wasm")).arrayBuffer();

	await audioContext.audioWorklet.addModule('realtime_worklet.js');
	audioSink = new AudioWorkletNode(audioContext, 'audio-sink');
	audioSink.port.postMessage(modules);
	audioSink.connect(analyser);
	analyser.connect(audioContext.destination);

	const scm = await Scheme.load_main("realtime_webgl.wasm", {}, {
		webaudio: {
			audioParamGet: (name) => audioSink.parameters.get(name),
			audioParamSet: (param, value) => param.setValueAtTime(value, audioContext.currentTime),
			audioParamMin: (param) => param.minValue,
			audioParamMax: (param) => param.maxValue,
			audioParamVal: (param) => param.value,
			audioContextState: () => audioContext.state,
			audioContextSuspend: () => audioContext.suspend(),
			audioContextResume: () => audioContext.resume()
		},
		window: {
			requestAnimationFrame: requestAnimationFrame,
			setTimeout: setTimeout
		},
		document: {
			getElementById: (id) => document.getElementById(id)
		},
		canvas: {
			getContext: (elem, type) => elem.getContext(type),
			setFillColor: (ctx, color) => ctx.fillStyle = color,
			setFont: (ctx, font) => ctx.font = font,
			setTextAlign: (ctx, align) => ctx.textAlign = align,
			clearRect: (ctx, x, y, w, h) => ctx.clearRect(x, y, w, h),
			fillRect: (ctx, x, y, w, h) => ctx.fillRect(x, y, w, h),
			fillText: (ctx, text, x, y) => ctx.fillText(text, x, y),
			drawImage: (ctx, image, sx, sy, sw, sh, dx, dy, dw, dh) => ctx.drawImage(image, sx, sy, sw, sh, dx, dy, dw, dh),
			setScale: (ctx, sx, sy) => ctx.scale(sx, sy),
			setTransform: (ctx, a, b, c, d, e, f) => ctx.setTransform(a, b, c, d, e, f),
			setImageSmoothingEnabled: (ctx, enabled) => ctx.imageSmoothingEnabled = (enabled == 1),
			beginPath: (ctx) => ctx.beginPath(),
			fill: (ctx) => ctx.fill(),
			stroke: (ctx) => ctx.stroke(),
			arc: (ctx, a, b, c, d, e) => ctx.arc(a, b, c, d, e, false),
			moveTo: (ctx, x, y) => ctx.moveTo(x, y),
			lineTo: (ctx, x, y) => ctx.lineTo(x, y),
			lineWidth: (ctx, w) => ctx.lineWidth = w,
			strokeStyle: (ctx, style) => ctx.strokeStyle = style,
			save: (ctx) => ctx.save(),
			restore: (ctx) => ctx.restore(),
			translate: (ctx, x, y) => ctx.translate(x, y)
		},
		element: {
			width: (el) => el.width,
			height: (el) => el.height
		},
		event: {
			addEventListener: (target, type, listener) => target.addEventListener(type, listener),
			preventDefault: (event) => event.preventDefault(),
			offsetX: (event) => event.offsetX,
			offsetY: (event) => event.offsetY
		},
		array: {
			newFloat32Array: (size) => new Float32Array(size),
			setFloat32Array: (a, i, v) => a[i] = v,
			getFloat32Array: (a, i) => a[i],
			bufFloat32Array: (a) => a.buffer
		},
		math: {
			random: Math.random
		},
		analyser: {
			getFloatTimeDomainData() {
				analyser.getFloatTimeDomainData(analyserData);
				return analyserData;
			},
			getRMS() {
				let m = 0;
				for (let i = 0; i < fftSize; i++) {
					const v = analyserData[i];
					m += v*v;
				}
				const rms = Math.sqrt(m);
				return rms;
			}
		}
	});

	for (const inp of document.querySelectorAll("input[name=difficulty]")) {
		inp.onchange = function() {
			const x = parseFloat(this.value);
			console.log(`setting difficulty to ${x}`);
			scm[0](x);
		}
	}
	
	/*
	  113.1
	  0.4
	  113.1
	  0

	  113.1
	  25.1
	  100.6 - 452.6
	  0 or 2

	  205.9
	  29.1
	  200.5
	  0 or 2

	  200.5
	  29.1
	  257.6
	  57.6, then 0.5 or 2
	  */


}

window.addEventListener("load", function() {
	// NOTE browsers generally require user interaction to start audio context
	let initOnce = false;
	const canvas = document.querySelector("#canvas");
	canvas.onclick = function() {
		if (!initOnce) {
			initOnce = true;
			gameInit();
		}
		canvas.onclick = null;
	};
	const ctx = canvas.getContext("2d");
	ctx.fillStyle = "#000";
	ctx.fillRect(0, 0, canvas.width, canvas.height);
	ctx.font = "bold 48px monospace";
	ctx.textAlign = "center";
	ctx.fillStyle = "#fff";
	ctx.fillText("CLICK TO START", canvas.width/2, canvas.height/2);
});
