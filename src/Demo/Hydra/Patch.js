"use strict";

// import Hydra from 'hydra-synth';
// const Hydra = require('hydra-synth');

import * as jsEnv from "browser-or-node";

var hydraInstance = null;
var lastExecuted = '';

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


const executeHydra_ = function(programString) {
    return function() {
        if (hydraInstance && (lastExecuted != programString)) {

            console.log('to execute:' + programString);
            // window.eval(programString);
            hydraInstance.sandbox.sandbox.eval(programString);
        }
    }
}

export const runHydra = runHydra_;
export const resize = resize_;
export const executeHydra = executeHydra_;