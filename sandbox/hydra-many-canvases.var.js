// CODEPEN: https://codepen.io/shamansir/pen/NPNjRbo?editors=1011

// https://cdn.jsdelivr.net/npm/virtual-webgl
// https://unpkg.com/hydra-synth

const INSTANCES_COLS = 3;
const INSTANCES_ROWS = 3;
const TOTAL_WIDTH = 200;
const TOTAL_HEIGHT = 200;
const INSTANCE_WIDTH  = TOTAL_WIDTH  / INSTANCES_COLS;
const INSTANCE_HEIGHT = TOTAL_HEIGHT / INSTANCES_ROWS;

const INSTANCES_COUNT = INSTANCES_COLS * INSTANCES_ROWS;

let masks = [];
let instances = [];

const hg = new Hydra({ makeGlobal : false, detectAudio: false, width : TOTAL_WIDTH, height : TOTAL_HEIGHT }).synth;

let index;
let x, y; let col, row;
let value;

console.log('------');

let start = hg.solid(1,0,1);

let scaleXFactor = 1 / INSTANCES_COLS;
let scaleYFactor = 1 / INSTANCES_ROWS;
let xPos, yPos;
let maskScrollX, maskScrollY;
let mask, instance;

console.log('cols', INSTANCES_COLS, 'rows', INSTANCES_ROWS, 'count', INSTANCES_COUNT);
console.log('scaleX', scaleXFactor, 'scaleY', scaleYFactor);

for (index = 0; index < INSTANCES_COUNT; index++) {
        value = index / (INSTANCES_COUNT - 1);
        col = index % INSTANCES_COLS;
        row = Math.floor(index / INSTANCES_COLS);
        xPos = col / INSTANCES_COLS;
        yPos = row / INSTANCES_ROWS;
        maskScrollX = (col + 0.5) / INSTANCES_COLS - 0.5;
        maskScrollY = (row + 0.5) / INSTANCES_ROWS - 0.5;
        console.log('index', index, 'value', value, 'x', x, 'y', y, 'xPos', xPos, 'yPos', yPos);
        mask = hg.shape(4, 1, 0.0001)
                   .scale(1, scaleXFactor, scaleYFactor)
                   .scrollX(maskScrollX)
                   .scrollY(maskScrollY);
        // instance = hg.osc(60, 0.1, value);
        // instance = hg.shape(2 + index)
        instance = hg.solid(0, 0, value);
                   // .scale(1, scaleXFactor, scaleYFactor)
                   // .scrollX(xPos)
                   // .scrollY(yPos);
        start.layer(instance.mask(mask));
}

start.out();