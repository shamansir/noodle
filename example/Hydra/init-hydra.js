const Hydra = require('hydra-synth');

console.log(Hydra);

var hydra = new Hydra({ detectAudio: false })
osc(4, 0.1, 1.2).out()