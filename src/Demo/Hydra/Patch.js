"use strict";

// import Hydra from 'hydra-synth';
// const Hydra = require('hydra-synth');

import * as jsEnv from "browser-or-node";

let hydraPatchInstance = null; // the instance to render global patch scene
let hydraNodesInstance = null; // the instance for rendering nodes' previews
let hg = hydraNodesInstance; // shorthand for calling `hydra` on `hydraNodesInstance`
let lastExecuted = '';
let lastWidth, lastHeight = 0;

const CURRENT_SCENE_CVS_ID = 'target-canvas';
const NODES_BODY_CVS_ID = 'nodes-body-canvas';

let start = null;


const runHydra_ = function() {
    if (!jsEnv.isBrowser) return;
    const targetCanvas    = document.getElementById(CURRENT_SCENE_CVS_ID);
    const nodesBodyCanvas = document.getElementById(NODES_BODY_CVS_ID);
    console.log(targetCanvas);
    hydraPatchInstance = new Hydra({ canvas : targetCanvas, detectAudio: false });
    hydraNodesInstance = new Hydra({ canvas : nodesBodyCanvas, makeGlobal : false, detectAudio: false });
    hg = hydraNodesInstance.synth;
    start = hg.solid(0,0,0,0);

    console.log(hydraPatchInstance);
    hydraScene_();
};


const resize_ = function (width) {
    return function (height) {
        return function() {
            lastWidth = width; lastHeight = height;
            if (!hydraPatchInstance || !hydraNodesInstance) return;
            hydraPatchInstance.setResolution(width, height);
            hydraNodesInstance.setResolution(width, height);
        }
    }
}


const executeHydra_ = function(programString) {
    return function() {
        if (hydraPatchInstance && (lastExecuted != programString)) {

            console.log('to execute:' + programString);
            // window.eval(programString);
            let evaluation = hydraPatchInstance.sandbox.sandbox.eval(programString);
            console.log('evaluation:', evaluation);
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

// const theMask = function() { return hg.shape(4, 1, 0.0001).scale(1, scaleXFactor, scaleYFactor); };
const theMask = function() { return hg.shape(999, 1.0, 0.6).scale(1, 0.5, 0.5); };


function positionAt_(left, top, scaleX, scaleY, whatFn) {
    scrollX = ((1.0 - left) - 0.5) - (scaleX / 2);
    scrollY = ((1.0 - top) - 0.5) - (scaleY / 2);
    positionByScroll_(scrollX, scrollY, scaleX, scaleY, whatFn);
}

function positionByScroll_(scrollX, scrollY, scaleX, scaleY, whatFn) {
    if (!hg || !start) return;
    console.log('scrollX', scrollX, 'scrollY', scrollY);
    mask = theMask();
    instance = whatFn();
    /*
    mask = theMask()
                   // .scale(1, scaleX, scaleY)
                   .scrollX(scrollX)
                   .scrollY(scrollY);
    instance = whatFn()
                   .scale(1, scaleX, scaleY)
                   .scrollX(scrollX)
                   .scrollY(scrollY);
    start.layer(instance.mask(mask));
    */
    start.layer(instance.mask(mask).scale(1, scaleX, scaleY).scrollX(scrollX).scrollY(scrollY));
    start.out(hg.o0);
}

let scaleX, scaleY = 1.0;
let posX, posY = 0.0;

const drawSceneAt_ = function(rect) {
    return function(what) {
        return function() {
            console.log('drawSceneAt', rect.left, rect.top, rect.width, rect.height, what);
            posX = rect.left / lastWidth;
            posY = rect.top / lastHeight;
            scaleX = rect.width / lastWidth;
            scaleY = rect.height / lastHeight;
            // positionAt_(rect.left, rect.top, 1.0 / rect.width, 1.0 / rect.height, function() { return hg.osc(60.0, 0.1, 1.0); });
            positionAt_(posX, posY, scaleX, scaleY, function() { return hg.osc(60.0, 0.1, 1.0); });
        }
    }

}


export const runHydra = runHydra_;
export const resize = resize_;
export const executeHydra = executeHydra_;
export const drawSceneAt = drawSceneAt_;