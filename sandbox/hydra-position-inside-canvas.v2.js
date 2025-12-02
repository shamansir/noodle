// https://codepen.io/shamansir/pen/JoXrGOo

const INSTANCES_COLS = 20;
const INSTANCES_ROWS = 25;
const TOTAL_WIDTH = 600;
const TOTAL_HEIGHT = 300;
const INSTANCE_WIDTH  = TOTAL_WIDTH  / INSTANCES_COLS;
const INSTANCE_HEIGHT = TOTAL_HEIGHT / INSTANCES_ROWS;

const INSTANCES_COUNT = INSTANCES_COLS * INSTANCES_ROWS;

let masks = [];
let instances = [];

const hg = new Hydra({ makeGlobal : false, detectAudio: false, width : TOTAL_WIDTH, height : TOTAL_HEIGHT }).synth;

console.log('------');

const start = hg.solid(1,0,1,0);
const theMask = function() { return hg.shape(4, 1, 0.0001) };

let index;
let x, y;
let col, row, revCol, revRow;
let value;
let scaleXFactor = 1 / INSTANCES_COLS;
let scaleYFactor = 1 / INSTANCES_ROWS;
let xPos, yPos;
let scrollX, scrollY;
let mask, instance;

console.log('cols', INSTANCES_COLS, 'rows', INSTANCES_ROWS, 'count', INSTANCES_COUNT);
console.log('scaleX', scaleXFactor, 'scaleY', scaleYFactor);

function positionAt(left, top, scaleX, scaleY, whatFn) {
    scrollX = ((1.0 - left) - 0.5) - (scaleX / 2);
    scrollY = ((1.0 - top) - 0.5) - (scaleY / 2);
    positionByScroll(scrollX, scrollY, scaleX, scaleY, whatFn);
}

function positionByScroll(scrollX, scrollY, scaleX, scaleY, whatFn) {
    console.log('scrollX', scrollX, 'scrollY', scrollY);
    mask = theMask()
                   .scale(1, scaleX, scaleY)
                   .scrollX(scrollX)
                   .scrollY(scrollY);
    instance = whatFn()
                   .scale(1, scaleX, scaleY)
                   .scrollX(scrollX)
                   .scrollY(scrollY);
    start.layer(instance.mask(mask));
}


function drawGrid() {
    for (index = 0; index < INSTANCES_COUNT; index++) {
            value = index / (INSTANCES_COUNT - 1);
            col = index % INSTANCES_COLS;
            row = Math.floor(index / INSTANCES_COLS);
            xPos = col / INSTANCES_COLS;
            yPos = row / INSTANCES_ROWS;
            revCol = INSTANCES_COLS - 1 - col;
            revRow = INSTANCES_ROWS - 1 - row;

            scrollX = ((revCol + 0.5) / INSTANCES_COLS) - 0.5;
            scrollY = ((revRow + 0.5) / INSTANCES_ROWS) - 0.5;

            console.log('index', index, 'value', value, 'xPos', xPos, 'yPos', yPos);
            console.log('col', col, 'row', row, 'revCol', revCol, 'revRow', revRow);

/*            positionByScroll(scrollX, scrollY, scaleXFactor, scaleYFactor, function() { return hg.shape(3 + index, 0.35, 0.1); }); */

        positionByScroll(scrollX, scrollY, scaleXFactor, scaleYFactor, function() { return hg.osc(0.06 * index, 0.01 * index, 0.1 * index); });

    }
}

drawGrid();
// positionAt(0.04, 0.3, 0.95, 0.5, function() { return hg.shape(3, 0.3, 0.09); });

start.out();