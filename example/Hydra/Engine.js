"use strict";

const Hydra = require('hydra-synth');

exports.init = function(canvasId) {
    return function() {
        console.log(document.getElementById(canvasId));
        new Hydra({ detectAudio: false, canvas: document.getElementById(canvasId) });
    }
}

exports.evaluate = function(hydraCode) {
    return function() {
        console.log(hydraCode);
        Function('"use strict";return (' + hydraCode + ')')();
    }
};