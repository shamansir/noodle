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
                console.log('~~what~~', JSON.stringify(what, null, 2));
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

const _unpackVal = function(h, v) {
    if (v.type === 'tex') {
        return _unpackTex(h, v.value);
    } else if (v.type === 'number') {
        return v.value;
    } else if (v.type === 'pi') {
        return h.pi;
    } else if (v.type === 'none') {
        return null;
    }
}

const _unpackTex = function(h, t) {
    if (t.type === 'empty') {
        return h.solid(0,0,0,0);
    } else if (t.type === 'start') {
        return _unpackStart(h, t.tex);
    } else if (t.type === 'geometry') {
        return _unpackApply(h, t.tex);
    } else if (t.type === 'color') {
        return _unpackApply(h, t.tex);
    } else if (t.type === 'blend') {
        return _unpackApplyWith(h, t.tex);
    } else if (t.type === 'modulate') {
        return _unpackApplyWith(h, t.tex);
    }
}

const _unpackStart = function(h, s) {
    if (s.type === 'from') {
        return _callSourceFn(h, s.src);
    } else if (s.type === 'load') {
        return _unpackOut(h, s.src);
    }
}

const _unpackApply = function(h, a) {
    const _what = _unpackTex(h, a.what);
    return _callApplyFn(h, _what, _with, a.fn);
}

const _unpackApplyWith = function(h, a) {
    const _what = _unpackTex(h, a.what);
    const _with = _unpackTex(h, a.with);
    return _callApplyWithFn(h, _what, _with, a.fn);
}

const _unpackOut = function(h, o) {
    if (o.out === 0) return h.o0;
    if (o.out === 1) return h.o1;
    if (o.out === 2) return h.o2;
    if (o.out === 3) return h.o3;
}

const _hydraSourceFn =
    {
        'osc': function(h, args) { return h.osc(args[0], args[1], args[2]); },
        'voronoi': function(h, args) { return h.voronoi(args[0], args[1], args[2]); },
        'noise': function(h, args) { return h.noise(args[0], args[1]); },
        'gradient': function(h, args) { return h.gradient(args[0]); },
        'solid': function(h, args) { return h.solid(args[0], args[1], args[2], args[3]); },
        'src': function(h, args) { return h.src(args[0]); }
    };

const _hydraApplyFn =
    {
        'color': function(h, _what, args) { return _what.color(args[0], args[1], args[2], args[3]); },
        'pixelate': function(h, _what, args) { return _what.pixelate(args[0], args[1], args[2], args[3]); }
    };

const _hydraApplyWithFn =
    {
        'blend': function(h, _what, _with, args) { return _what.blend(_with, args[0]); }
    };

const _callSourceFn = function(h, fn) {
    if (!_hydraSourceFn.hasOwnProperty(fn.name)) {
        console.warn('Unknown hydra source fn:', fn.name);
        return null;
    }
    return _hydraSourceFn[fn.name](h, fn.args.map((arg) => _unpackVal(h, arg.value)));
}

const _callApplyFn = function(h, _what, fn) {
    if (!_hydraApplyFn.hasOwnProperty(fn.name)) {
        console.warn('Unknown hydra apply fn:', fn.name);
        return null;
    }
    return _hydraApplyFn[fn.name](h, _what, fn.args.map((arg) => _unpackVal(h, arg.value)));
}

const _callApplyWithFn = function(h, _what, _with, fn) {
    if (!_hydraApplyWithFn.hasOwnProperty(fn.name)) {
        console.warn('Unknown hydra apply-with fn:', fn.name);
        return null;
    }
    return _hydraApplyWithFn[fn.name](h, _what, _with, fn.args.map((arg) => _unpackVal(h, arg.value)));
}

export const h_runHydra = runHydra_;
export const h_resize = resize_;
export const h_executeHydra = executeHydra_;
export const h_drawNodeSceneOf = drawNodeSceneOf_;
export const h_clearNodeScenes = clearNodeScenes_;