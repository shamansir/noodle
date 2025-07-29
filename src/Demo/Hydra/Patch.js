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
    hydraScene_();
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

const hydraScene_ = function() {
    // osc(4, 0.1, 1.2).out();
    var n = 50;
    var func = () => osc(30,0.1,1).modulate(noise(4,0.1));
    var pix = () => shape(4,0.3).scale(1,1,3).repeat(n,n);
    pix().mult(func().color(1,0,0).pixelate(n,n)).out(o1);
    pix().mult(func().color(0,1,0).pixelate(n,n)).scrollX(1/n/3).out(o2);
    pix().mult(func().color(0,0,1).pixelate(n,n)).scrollX(2/n/3).out(o3);

    solid().add(src(o1),1).add(src(o2),1).add(src(o3),1).out(o0);
}


export const runHydra = runHydra_;
export const resize = resize_;
export const executeHydra = executeHydra_;