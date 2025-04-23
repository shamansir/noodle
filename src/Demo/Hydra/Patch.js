"use strict";

import Hydra from 'hydra-synth';

const runHydra_ = function() {
    return function() {
        var hydra = new Hydra({ detectAudio: false });
        osc(4, 0.1, 1.2).out();
    }
};

export const runHydra = runHydra_;