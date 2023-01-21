var blessed = require('blessed');
const { init } = require('hydra-synth/src/lib/array-utils');

// Create a screen object.
var screen = blessed.screen({
  smartCSR: true,
  // terminal: 'xterm-256color',
  fullUnicode: true
});

screen.title = 'Noodle';

let lastClickedOutlet = null;

const PALETTE =
  [ '#111' // 0; background
  , '#006600' // 1; item not selected
  , '#00ff00' // 2; item selected
  , '#f0f0f0' // 3; border
  , '#666' // 4; node list fg
  , 'white' // 5; node list selected fg, focused border
  , 'blue' // 6; node box border
  , '#000033' // 7; family marker
  , 'green' // 8; link color
  ]

const patches = [ 'Patch1', 'Patch2', '+' ];
const patchesBar = blessed.listbar({
  top: 0,
  left : 0,
  width : '100%',
  height : 1,
  items : patches,
  mouse: true,
  keys : true,
  style : { bg : PALETTE[0], item : { fg : PALETTE[1], bg : PALETTE[0] }, selected : { fg : PALETTE[2], bg : PALETTE[0] } }
});
screen.append(patchesBar);


// Create a box perfectly centered horizontally and vertically.
var patchBox = blessed.box({
  top: 'center+1',
  left: 'center',
  width: '100%',
  height: '100%-1',
  content: 'Patch',
  tags: true,
  border: {
    type: 'line'
  },
  style: {
    fg: 'white',
    bg: 'black',
    border: {
      fg: PALETTE[3]
    }/*,
    hover: {
      bg: '#111'
    } */
  }
});

function log(text) {
  patchBox.setContent('                ' + text);
}

patchBox.on('focus', () => {
  lastClickedOutlet = null;
});

patchBox.on('click', () => {
  lastClickedOutlet = null;
});

const items = [ 'foo', 'bar', 'ololo', 'hello', 'foo1', 'bar1', 'ololo1', 'hello1', 'foo2', 'bar2', 'ololo2', 'hello2' ]

nodeList = blessed.list({
  top : 0,
  left : 0,
  width : 14,
  height : '40%',
  draggable : true,
  scrollable : true,
  items : items,
  // interactive : true,
  mouse : true,
  keys : true,
  border : { type : 'line', fg : PALETTE[4] },
  style : {
      selected : { fg : PALETTE[5] },
      item : { fg : PALETTE[4] }
  }
});

let lastShiftX = -1;
let lastShiftY = -1;

let nodes = {};
let links = {};
let lastNodeId = 0;

nodeList.on('select', (item, index) => {

  log(nodeList.selected + ':' + index + ':' + items[nodeList.selected]);

  const top = lastShiftX + 2;
  const left = 16 + lastShiftY + 2;

  const nodeId = lastNodeId;

  const nodeBox = blessed.box({
    // content : items[nodeList.selected],
    draggable : true,
    top : top,
    left : left,
    width : 25,
    height : 5,
    border : { type : 'line', fg : PALETTE[6], ch : '∶' },
    style :
        { focus : { border : { fg : PALETTE[5] } }
        }
  });

  lastShiftX = lastShiftX + 1;
  lastShiftY = lastShiftY + 1;

  const is = [ 'a', 'b', 'c' ];
  const os = [ 'sum', 'x' ];

  const inlets = blessed.listbar({
    width : '90%',
    height : 1,
    items : is,
    mouse: true,
    keys : true,
    style : { bg : '#111', item : { fg : PALETTE[1], bg : PALETTE[0] }, selected : { fg : PALETTE[1], bg : PALETTE[0] } }
  });

  const outlets = blessed.listbar({
    width : '90%',
    top : 2,
    height : 1,
    items : os,
    mouse: true,
    keys : true,
    style : { bg : '#111', item : { fg : PALETTE[1], bg : PALETTE[0] }, selected : { fg : PALETTE[1], bg : PALETTE[0] } }
  });

  const family = items[nodeList.selected];

  nodes[nodeId] = { box : nodeBox, top, left, inlets, outlets, family, index : nodeId };

  nodeBox.data = { index : nodeId };

  inlets.on('select', (item, index) => {

    const selectedInlet = parseInt(item.getText().split(':')[0]) - 1; // not inlets.selected or index!

    if (lastClickedOutlet && (lastClickedOutlet.node != nodeId)) {
      const fromNode = lastClickedOutlet.node;
      const toNode = nodes[nodeId].index;
      // TODO: proceed only if there are no links exist between these input / output already

      const link = new Link(fromNode, lastClickedOutlet.index, toNode, selectedInlet);
      link.add(patchBox);

      link.on('click', () => {
        link.remove(patchBox);
        screen.render();
      });

      lastClickedOutlet = null;
      if (!links[fromNode]) { links[fromNode] = [] };
      links[fromNode].push(link);
      if (!links[toNode]) { links[toNode] = [] };
      links[toNode].push(link);
      screen.render();

    }
  });

  outlets.on('select', (item, index) => {
    const selectedOutlet = parseInt(item.getText().split(':')[0]) - 1; // not outlets.selected or index!
    log('O' + nodeId + ':' + outlets.selected + ':' + index + ' ' + item.getText() + '      ' + selectedOutlet);
    lastClickedOutlet = { node : nodeId, index : selectedOutlet, subj : os[selectedOutlet] };
    screen.render();
  });

  lastNodeId = lastNodeId + 1;

  nodeBox.append(inlets);
  nodeBox.append(outlets);

  patchBox.append(nodeBox);

  nodeBox.on('move', () => {
    if (links[nodeId]) {
      links[nodeId].forEach((link) => {
        link.update();
        screen.render();
      });
    }
  });

  nodeBox.setLine(1, blessed.parseTags('{' + PALETTE[7] + '-fg}>{/' + PALETTE[7] + '-fg} ' + family));

  screen.render();
});

patchBox.append(nodeList);

screen.append(patchBox);

// Quit on Escape, q, or Control-C.
screen.key(['escape', 'q', 'C-c'], function(ch, key) {
  return process.exit(0);
});

nodeList.focus();

screen.render();



class Link {

  constructor(fromNode, outletIndex, toNode, inletIndex) {
    this.fromNode = fromNode;
    this.outletIndex = outletIndex;
    this.toNode = toNode;
    this.inletIndex = inletIndex;
    const calc = calcLink(fromNode, outletIndex, toNode, inletIndex);
    this.init(calc);
  }

  init(calc) {
    this.link = {};
    this.link.a = blessed.line({ left : calc.a.left, top : calc.a.top, width : calc.a.width, height : calc.a.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });
    this.link.b = blessed.line({ left : calc.b.left, top : calc.b.top, width : calc.b.width, height : calc.b.height, orientation : 'horizontal', type : 'bg', ch : '∼', fg : PALETTE[8] });
    this.link.c = blessed.line({ left : calc.c.left, top : calc.c.top, width : calc.c.width, height : calc.c.height, orientation : 'vertical', type : 'bg', ch : '≀', fg : PALETTE[8] });
  }

  add(patchBox) {
    patchBox.append(this.link.a);
    patchBox.append(this.link.b);
    patchBox.append(this.link.c);
  }

  remove(patchBox) {
    patchBox.remove(this.link.a);
    patchBox.remove(this.link.b);
    patchBox.remove(this.link.c);
  }

  on(event, handler) {
    this.link.a.on(event, handler);
    this.link.b.on(event, handler);
    this.link.c.on(event, handler);
  }

  update(patchBox) {
    const calc = calcLink(this.fromNode, this.outletIndex, this.toNode, this.inletIndex);

    this.link.a.left = calc.a.left;
    this.link.a.top = calc.a.top;
    this.link.a.width = calc.a.width;
    this.link.a.height = calc.a.height;

    this.link.b.left = calc.b.left;
    this.link.b.top = calc.b.top;
    this.link.b.width = calc.b.width;
    this.link.b.height = calc.b.height;

    this.link.c.left = calc.c.left;
    this.link.c.top = calc.c.top;
    this.link.c.width = calc.c.width;
    this.link.c.height = calc.c.height;
  }

}


function calcLink(scrNodeIndex, outletIndex, dstNodeIndex, inletIndex) {
    const outletNode = nodes[scrNodeIndex];
    const inletNode = nodes[dstNodeIndex];
    const outletPosX = outletNode.box.left + (outletIndex * 6);
    const outletPosY = outletNode.box.top + 3;
    const inletPosX = inletNode.box.left + (inletIndex * 6)
    const inletPosY = inletNode.box.top + 1;
    let xo = outletPosX, yo = outletPosY;
    let xi = inletPosX, yi = inletPosY;
    let my = Math.floor(Math.abs(yi - yo) / 2);
    let lineA, lineB, lineC;
    if (outletPosY <= inletPosY) { // outlet is higher than inlet,
        lineA = { left : xo, top : yo, width : 1, height : my };
        if (outletPosX <= inletPosX) { // outlet is on the left side from the inlet
          lineB = { left : xo, top : yo + my, width : xi - xo, height : 1 };
        } else { // outlet is on the right side from the inlet
          lineB = { left : xi, top : yo + my, width : xo - xi, height : 1 };
        }
        lineC = { left : xi, top : yo + my, width : 1, height : my };
    } else { // outlet is lower than inlet
        lineA = { left : xi, top : yi, width : 1, height : my };
        if (inletPosX <= outletPosX) { // inlet is on the left side from the outlet
          lineB = { left : xi, top : yi + my, width : xo - xi, height : 1 };
        } else {  // inlet is on the right side from the outlet
          lineB = { left : xo, top : yi + my, width : xi - xo, height : 1 };
        }
        lineC = { left : xo, top : yi + my, width : 1, height : my };
    }
    log(scrNodeIndex + ':' + outletIndex + ' ' + dstNodeIndex + ':' + inletIndex + ' xo:' + xo + ' yo: ' + yo + ' xi:' + xi + ' yi: ' + yi);
    // ⊲ ⊳ ⋎ ⋏ ≺ ≻ ⊽ ⋀ ⋁ ∻ ∶ ∼ ∽ ∾ ∷ ∻ ∼ ∽ ≀ ⊶ ⊷ ⊸ ⋮ ⋯ ⋰ ⋱ ⊺ ⊢ ⊣ ⊤ ⊥ ⊦ ∣ ∤ ∥ ∦ ∗ ∘ ∙ ⋄ ⋅ ⋆ ⋇ > ⋁
    return { a : lineA, b : lineB, c : lineC };
}