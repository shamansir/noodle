"use strict";

import Hydra from 'hydra-synth';

export const init = function(canvasId) {
    return function() {
        console.log(document.getElementById(canvasId));
        new Hydra({ detectAudio: false, canvas: document.getElementById(canvasId) });
    }
}

export const evaluate = function(hydraCode) {
    return function() {
        console.log(hydraCode);
        Function('"use strict";return (function(){' + hydraCode + '})')()();
    }
};