// CODEPEN: https://codepen.io/shamansir/pen/YPqZJOz

// https://cdn.jsdelivr.net/npm/virtual-webgl
// https://unpkg.com/hydra-synth

const CANVAS_COUNT = 16;
const WIDTH = 50;
const HEIGHT = 50;

let canvases = [];

for (var i = 0; i < CANVAS_COUNT; i++) {
    canvases[i] = document.getElementById(i < 10 ? ('canvas-0' + i) : ('canvas-' + i));
}

/*     [ document.getElementById('canvas-00'), document.getElementById('canvas-01'), document.getElementById('canvas-02'), document.getElementById('canvas-03'), document.getElementById('canvas-04'), document.getElementById('canvas-05')
     // , document.getElementById('canvas-06')
     ]; */

/* const hg = new Hydra({ makeGlobal : false, detectAudio: false, width : 50, height : 50 }).synth;
hg.osc(4, 0.1, 1.2).out(); */

let hydras = [];

for (var i = 0; i < CANVAS_COUNT; i++) {
   hydras[i] = new Hydra({ canvas : canvases[i], makeGlobal : false, detectAudio: false, width : WIDTH, height : HEIGHT }).synth;
   hydras[i].osc(4, 0.1, 1.2 * (i / CANVAS_COUNT)).out();
}

/*
const h0 = new Hydra({ canvas : canvases[0], makeGlobal : false, detectAudio: false, width : 50, height : 50 }).synth;
h0.osc(4, 0.1, 1.2).out();
*/