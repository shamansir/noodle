"use strict";

// import Hydra from 'hydra-synth';
// const Hydra = require('hydra-synth');

const runHydra_ = function() {
    const targetCanvas = document.getElementById('target-canvas');
    console.log(targetCanvas);
    const hydra = new Hydra({ canvas : targetCanvas, detectAudio: false });
    console.log(hydra);
    osc(4, 0.1, 1.2).out();
};

export const runHydra = runHydra_;