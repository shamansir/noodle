"use strict";

import * as jsEnv from "browser-or-node";

console.log(jsEnv.isBrowser);

// import Hydra from 'hydra-synth';
// const Hydra = require('hydra-synth');

var hydraInstance = null;

const runHydra_ = function() {
    if (!jsEnv.isBrowser) return;
    const targetCanvas = document.getElementById('target-canvas');
    console.log(targetCanvas);
    hydraInstance = new Hydra({ canvas : targetCanvas, detectAudio: false });
    console.log(hydraInstance);
    osc(4, 0.1, 1.2).out();
};


const resize_ = function (width) {
    return function (height) {
        return function() {
            if (!hydraInstance) return;
            hydraInstance.setResolution(width, height);
        }
    }
}

export const runHydra = runHydra_;
export const resize = resize_;