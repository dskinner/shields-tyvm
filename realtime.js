const audioContext = new AudioContext({sampleRate: 44100});
const analyser = audioContext.createAnalyser();
analyser.fftSize = 2048
const analyserData = new Float32Array(2048);
const waveformData = new Float32Array(4096);

window.addEventListener("load", async function() {
    const canvas = document.querySelector("#glcanvas");
    const webgl = canvas.getContext("webgl");
    if (webgl === null) {
        return console.error("unable to init webgl");
    }

    const bindings = {};
    const skiplist = ["canvas", "drawingBufferWidth", "drawingBufferHeight"];
    for (const name of Object.getOwnPropertyNames(WebGLRenderingContext.prototype)) {
        if (skiplist.includes(name)) {
            continue;
        }
        const prop = WebGLRenderingContext.prototype[name];
        if (typeof(prop) === "function") {
            // e.g. {clearColor: WebGLRenderingContext.prototype.clearColor.bind(webgl),
            //            clear: WebGLRenderingContext.prototype.clear.bind(webgl)}
            bindings[name] = prop.bind(webgl);
        }
    }

    await Scheme.load_main("realtime_webgl.wasm", {}, {
        webgl: bindings,
        window: {
            requestAnimationFrame: requestAnimationFrame
        },
        analyser: {
            getFloatTimeDomainData() {
                // TODO not clear how best to share array here so process in js for now
                analyser.getFloatTimeDomainData(analyserData);
                for (let i = 0; i < 2048; i++) {
                    waveformData[i*2] = (i/2048)*4-1;
                    waveformData[i*2+1] = analyserData[i];
                }
                return waveformData;
            }
        }
    });

    let modules = {};
    modules["js-runtime/wtf8.wasm"] = await WebAssembly.compileStreaming(fetch("js-runtime/wtf8.wasm"));
    modules["js-runtime/reflect.wasm"] = await WebAssembly.compileStreaming(fetch("js-runtime/reflect.wasm"));
    modules["realtime_audio.wasm"] = await WebAssembly.compileStreaming(fetch("realtime_audio.wasm"));

    await audioContext.audioWorklet.addModule('realtime_worklet.js');
    const audioSink = new AudioWorkletNode(audioContext, 'audio-sink');
    audioSink.port.postMessage(modules);
	audioSink.connect(analyser);
    analyser.connect(audioContext.destination);

    document.querySelector("#toggleaudio").onclick = function() { audioContext.state === "running" ? audioContext.suspend() : audioContext.resume(); };

	// TODO below can all probably move to scm
	
	const freqParam = audioSink.parameters.get("freq");
	document.querySelector("#freq").oninput = function() {
		document.querySelector("#lblfreq").innerText = this.value + "hz";
		freqParam.setValueAtTime(this.value, audioContext.currentTime);
	};

	const modParam = audioSink.parameters.get("mod");
	document.querySelector("#mod").oninput = function() {
		document.querySelector("#lblmod").innerText = this.value + "hz";
		modParam.setValueAtTime(this.value, audioContext.currentTime);
	};

	const uc0Param = audioSink.parameters.get("uc0");
	document.querySelector("#uc0").oninput = function() {
		document.querySelector("#lbluc0").innerText = this.value + "hz";
		uc0Param.setValueAtTime(this.value, audioContext.currentTime);
	};

	const uc1Param = audioSink.parameters.get("uc1");
	document.querySelector("#uc1").oninput = function() {
		document.querySelector("#lbluc1").innerText = this.value + "hz";
		uc1Param.setValueAtTime(this.value, audioContext.currentTime);
	};

	addEventListener("keydown", function(ev) {
		if (ev.keyCode != 32) {
			return;
		}
		uc1Param.setValueAtTime(2.0, audioContext.currentTime);
	});

	addEventListener("keyup", function(ev) {
		if (ev.keyCode != 32) {
			return;
		}
		uc1Param.setValueAtTime(0.5, audioContext.currentTime);
	});


	/*
	  113.1
	  0.4
	  113.1
	  0

	  maybe a blast changes shield freq permanently and then modulator falls and recovers over time,
	  then goal is to find the new frequency by ear and fixing phase by turning 2nd modulator on/off

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

});
