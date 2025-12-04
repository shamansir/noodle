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

let targetCanvas, nodesBodyCanvas = null;

let start = null;


const runHydra_ = function() {
    if (!jsEnv.isBrowser) return;
    targetCanvas    = document.getElementById(CURRENT_SCENE_CVS_ID);
    nodesBodyCanvas = document.getElementById(NODES_BODY_CVS_ID);
    console.log(targetCanvas);
    hydraPatchInstance = new Hydra({ canvas : targetCanvas, detectAudio: false });
    hydraNodesInstance = new Hydra({ canvas : nodesBodyCanvas, makeGlobal : false, detectAudio: false });
    hg = hydraNodesInstance.synth;
    start = hg.solid(0,0,0,0);

    console.log(hydraPatchInstance);
    _hydraScene();
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

const _hydraScene = function() {
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
const _ovalMask = function() { return hg.shape(999, 1.0, 0.6).scale(1, 0.5, 0.5); };


function _positionAt(rect, maskFn, whatFn) {
    // scrollX = ((1.0 - left) - 0.5) - (scaleX / 2);
    // scrollY = ((1.0 - top) - 0.5) - (scaleY / 2);

    _positionByScroll(rect, maskFn, whatFn);
}

function _positionByScroll(rect, maskFn, whatFn) {
    if (!hg || !start) return;
    mask = maskFn();
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
    //start.layer(instance.mask(mask).scale(1, rect.width, rect.height).scrollX(scroll.x).scrollY(scroll.y));
    start.layer(_transformInstance(instance.mask(mask), rect));
}

let scaleX, scaleY = 1.0;
let posX, posY = 0.0;
let nodeRect, nodeBodyRect = null;

function _transformBounds(rect, toWidth, toHeight) {
    return {
        left: rect.left / toWidth,
        top: rect.top   / toHeight,
        width: rect.width  / toWidth,
        height: rect.height / toHeight
    };
}

function _scrollParams(rect) {
    return { x : ((1.0 - rect.left) - 0.5) - (rect.width / 2), y: ((1.0 - rect.top) - 0.5) - (rect.height / 2) };
}

function _transformInstance(instance, rect) {
    return _transformInstanceScroll(instance, _scrollParams(rect), rect);
}

function _transformInstanceScroll(instance, scroll, rect) {
    console.log('scrollX', scroll.x, 'scrollY', scroll.y);
    return instance.scale(1, rect.width, rect.height).scrollX(scroll.x).scrollY(scroll.y);
}

const drawNodeSceneOf_ = function (nodeId) {
    return function(geom) {
        return function(what) {
            return function() {
                let nodeRect = _transformBounds(geom.node, lastWidth, lastHeight);
                let nodeBodyRect = _transformBounds(geom.body, lastWidth, lastHeight);
                // start.layer(hg.solid(1,1,1,1));
                start.mask(hg.solid(1,1,1,1).layer(_transformInstance(hg.shape(4,1.0), nodeRect)).invert());
                console.log('drawSceneAt', nodeId, geom, what);
                // start.mask(hg.shape(4,0.6).invert().color(1,1,1,1));
                // start.mask(hg.solid(1,1,1,1).layer(hg.shape(4,1.0).scale(1, rect.width, rect.height).scrollX()).invert());
                _positionAt(nodeBodyRect, _ovalMask, function() { return hg.osc(60.0, 0.1, 1.0); });
                // start.mask(hg.shape(4,0.6).invert().color(1,1,1,1));
                //hg.shape(4,0.6).invert().color(1,1,1,1);
                start.out(hg.o0);
            }
        }
    }
}

const clearNodeScenes_ = function() {
    if (!start) return;
    start = hg.solid(0,0,0,0);
    start.out(hg.o0);
    // const ctx = nodesBodyCanvas.getContext('webgl');
    // ctx.clear(ctx.COLOR_BUFFER_BIT | ctx.DEPTH_BUFFER_BIT);
    // ctx.clearRect(0, 0, nodesBodyCanvas.width, nodesBodyCanvas.height);
}


export const h_runHydra = runHydra_;
export const h_resize = resize_;
export const h_executeHydra = executeHydra_;
export const h_drawNodeSceneOf = drawNodeSceneOf_;
export const h_clearNodeScenes = clearNodeScenes_;