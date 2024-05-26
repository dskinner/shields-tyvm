const audioContext = new AudioContext({sampleRate: 8000});
const analyser = audioContext.createAnalyser();
analyser.fftSize = 2048
const analyserData = new Float32Array(2048);
const waveformData = new Float32Array(4096);

let difficulty = 8.0;

function getRMS() {
	let m = 0;
	for (let i = 0; i < 2048; i++) {
		const v = analyserData[i];
		m += v*v;
	}
	const rms = Math.sqrt(m);
	return rms;
}

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

    const scm = await Scheme.load_main("realtime_webgl.wasm", {}, {
        webgl: bindings,
        window: {
            requestAnimationFrame: requestAnimationFrame
        },
        analyser: {
            getFloatTimeDomainData() {
                // TODO not clear how best to share array here so process in js for now
                analyser.getFloatTimeDomainData(analyserData);
                for (let i = 0; i < 2048; i++) {
                    waveformData[i*2] = (i/2048)*4-1;      // x
                    waveformData[i*2+1] = analyserData[i]; // y
                }
                return waveformData;
            },
			getRMS() {
				let m = 0;
				for (let i = 0; i < 2048; i++) {
					const v = analyserData[i];
					m += v*v;
				}
				const rms = Math.sqrt(m);
				return rms;
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

	function mapAudioParam(name) {
		const param = audioSink.parameters.get(name);
		const label = document.querySelector(`#lbl${name}`);
		const input = document.querySelector(`#${name}`);

		input.min = param.minValue;
		input.max = param.maxValue;

		label.innerText = param.value + "hz";
		input.value = param.value
		input.oninput = function() {
			label.innerText = this.value + "hz";
			param.setValueAtTime(this.value, audioContext.currentTime);
		};

		return {param, label, input};
	}

	const freq = mapAudioParam("freq");
	const modfreq = mapAudioParam("modfreq");
	const modphase = mapAudioParam("modphase");

	function setAudioParam(o, v) {
		o.param.setValueAtTime(v, audioContext.currentTime);
		o.label.innerText = o.param.value.toFixed(3) + "hz";
		o.input.value = o.param.value;
	}

	function setAudioParamRandom(o) {
		setAudioParam(o, randomFloat(o.param.minValue, o.param.maxValue));
	}

	function randomFloat(s, e) {
		return s + (Math.random()*(e-s));
	}

	function generateAttack() {
		setAudioParamRandom(freq);
		setAudioParamRandom(modfreq);
		setAudioParamRandom(modphase);
	}

	// modphase.input.onchange = function() {
	// 	setAudioParam(modphase, 0.0);
	// };

	addEventListener("keydown", function(ev) {
		if (ev.keyCode == 32) {
			setAudioParam(modphase, 0.0);
		}

		if (ev.keyCode == 82) {
			generateAttack();
		}
	});


	const incoming = document.querySelector("#incoming");
	const damage = document.querySelector("#damage");
	const survived = document.querySelector("#survived");
	const perfect = document.querySelector("#perfect");
	let dmg = 0;
	let iter = 0;
	let surv = 0;
	let perf = 0;

	const countdown = 10;
	scm[1](difficulty);

	for (const inp of document.querySelectorAll("input[name=difficulty]")) {
		inp.onchange = function() {
			const x = parseFloat(this.value);
			console.log(`setting difficulty to ${x}`);
			difficulty = x;
			scm[1](difficulty);
		}
	}
	
	function lp() {
		iter++;
		const n = (countdown-1)-(iter%countdown);
		incoming.innerText = `Incoming in ${n} seconds!`;
		if (n == 0) {
			const rms = getRMS();
			if (rms >= difficulty) {
				// dmg += 2*(rms-difficulty);
				dmg += 1.5*rms; // penalty
			} else if (rms >= 1.0) {
				dmg += rms/2;
			} else {
				perf += 1;
				// dmg += (rms-difficulty)/2; // recover some shields
				// if (dmg < 0) { dmg = 0; }
			}
			damage.innerText = `Damage: ${dmg.toFixed(3)}%`
			perfect.innerText = `Perfectly thwarted ${perf} attack(s)!`;
			if (dmg < 100) {
				generateAttack();
				surv++;
				survived.innerText = `Survived ${surv} attack(s)!`;

			} else {
				// game over
				audioContext.suspend();
			}
		}
		if (dmg < 100) {
			setTimeout(lp, 1000);
		}
	}
	setTimeout(lp, 1000);
	

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

});
