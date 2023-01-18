var blessed = require('blessed');

// Create a screen object.
var screen = blessed.screen({
  smartCSR: true,
  // terminal: 'xterm-256color',
  fullUnicode: true
});

screen.title = 'Noodle';

let lastClickedOutlet = null;

// Create a box perfectly centered horizontally and vertically.
var patchBox = blessed.box({
  top: 'center',
  left: 'center',
  width: '100%',
  height: '100%',
  content: 'Patch',
  tags: true,
  border: {
    type: 'line'
  },
  style: {
    fg: 'white',
    bg: 'black',
    border: {
      fg: '#f0f0f0'
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
  border : { type : 'line', fg : '#666' },
  style : {
      selected : { fg : 'white' },
      item : { fg : '#666' }
  }
});

let lastShiftX = -1;
let lastShiftY = -1;

let nodes = {};
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
    border : { type : 'line', fg : 'blue', ch : '∶' },
    style :
        { focus : { border : { fg : 'white' } }
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
    style : { bg : '#111', item : { fg : '#006600', bg : '#111' }, selected : { fg : '#00ff00', bg : '#111' } }
  });

  const outlets = blessed.listbar({
    width : '90%',
    top : 2,
    height : 1,
    items : os,
    mouse: true,
    keys : true,
    style : { bg : '#111', item : { fg : '#006600', bg : '#111' }, selected : { fg : '#00ff00', bg : '#111' } }
  });

  const family = items[nodeList.selected];

  nodes[nodeId] = { box : nodeBox, top, left, inlets, outlets, family, index : nodeId };

  nodeBox.data = { index : nodeId };

  inlets.on('select', (item, index) => {

    const selectedInlet = parseInt(item.getText().split(':')[0]) - 1; // not inlets.selected or index!
    log('I' + nodeId + ':' + inlets.selected + ':' + index + ' ' + item.getText() + '      ' + selectedInlet);
    if (lastClickedOutlet && (lastClickedOutlet.node != nodeId)) {
      const link = buildLink(lastClickedOutlet.node, lastClickedOutlet.index, nodes[nodeId].index, selectedInlet);
      patchBox.append(link.a);
      patchBox.append(link.b);
      lastClickedOutlet = null;
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

  nodeBox.setLine(1, blessed.parseTags('{#000033-fg}>{/#000033-fg} ' + family));

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


function buildLink(scrNodeIndex, outletIndex, dstNodeIndex, inletIndex) {
    const outletNode = nodes[scrNodeIndex];
    const inletNode = nodes[dstNodeIndex];
    const outletPosX = outletNode.box.aleft + (outletIndex * 4);
    const outletPosY = outletNode.box.atop + 3;
    const inletPosX = inletNode.box.aleft + (inletIndex * 4)
    const inletPosY = inletNode.box.atop + 3;
    const lineStartX = Math.min(outletPosX, inletPosX);
    const lineStartY = Math.min(outletPosY, inletPosY);
    const lineEndX = Math.max(outletPosX, inletPosX);
    const lineEndY = Math.max(outletPosY, inletPosY);
    log(scrNodeIndex + ':' + outletIndex + ' ' + dstNodeIndex + ':' + inletIndex + ' sx:' + lineStartX + ' sy:' + lineStartY + ' ex:' + lineEndX + ' ey:' + lineEndY);
    // ⊲ ⊳ ⋎ ⋏ ≺ ≻ ⊽ ⋀ ⋁ ∻ ∶ ∼ ∽ ∾ ∷ ∻ ∼ ∽ ≀ ⊶ ⊷ ⊸ ⋮ ⋯ ⋰ ⋱ ⊺ ⊢ ⊣ ⊤ ⊥ ⊦ ∣ ∤ ∥ ∦ ∗ ∘ ∙ ⋄ ⋅ ⋆ ⋇ > ⋁
    const lineA = blessed.line({ left : lineStartX, top : lineStartY, width : lineEndX - lineStartX, height : 1, orientation : 'horizontal', type : 'bg', ch : '∼', fg : 'green' });
    const lineB = blessed.line({ left : lineEndX, top : lineStartY, width : 1, height : lineEndY - lineStartY, orientation : 'vertical', type : 'bg', ch : '≀', fg : 'green' });
    return { a : lineA, b : lineB };
}