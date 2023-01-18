var blessed = require('blessed');

// Create a screen object.
var screen = blessed.screen({
  smartCSR: true
});

screen.title = 'Noodle';

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
    },
    hover: {
      bg: '#111'
    }
  }
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

nodeList.on('select', () => {
  nodeBox = blessed.box({
    // content : items[nodeList.selected],
    draggable : true,
    top : lastShiftX + 2,
    left : 16 + lastShiftY + 2,
    width : 25,
    height : 5,
    border : { type : 'line', fg : 'blue' },
    style :
        { focus : { border : { fg : 'white' } }
        }
  });

  lastShiftX = lastShiftX + 1;
  lastShiftY = lastShiftY + 1;

  const inlets = blessed.listbar({
    width : '90%',
    height : 1,
    items : [ 'a', 'b', 'c' ],
    mouse: true,
    style : { item : { fg : '#006600' }, selected : { fg : '#00ff00' } }
  });

  const outlets = blessed.listbar({
    width : '90%',
    top : 2,
    height : 1,
    items : [ 'sum' ],
    mouse: true,
    style : { item : { fg : '#006600' }, selected : { fg : '#00ff00' } }
  });

  nodeBox.append(inlets);
  nodeBox.append(outlets);

  patchBox.append(nodeBox);

  nodeBox.setLine(1, blessed.parseTags('{blue-fg}>{/blue-fg} ' + items[nodeList.selected]));

  //patchBox.setContent('OOOOOOOOOOOOOOOOOOOOOOOOOO' + nodeList.selected);
  screen.render();
});

patchBox.append(nodeList);

lineA = blessed.line({ top : 3, left : 0, width : 50, height : 1, orientation : 'horizontal', type : 'bg', ch : '>', fg : 'green' });
lineB = blessed.line({ top : 3, left : 50, width : 1, height : 50, orientation : 'vertical', type : 'bg', ch : 'v', fg : 'green' });
patchBox.append(lineA);
patchBox.append(lineB);

// Append our box to the screen.
screen.append(patchBox);

/*
// If our box is clicked, change the content.
patchBox.on('click', function(data) {
  box.setContent('{center}Some different {red-fg}content{/red-fg}.{/center}');
  screen.render();
});

// If box is focused, handle `enter`/`return` and give us some more content.
patchBox.key('enter', function(ch, key) {
  box.setContent('{right}Even different {black-fg}content{/black-fg}.{/right}\n');
  box.setLine(1, 'bar');
  box.insertLine(1, 'foo');
  screen.render();
});
*/


// Quit on Escape, q, or Control-C.
screen.key(['escape', 'q', 'C-c'], function(ch, key) {
  return process.exit(0);
});

// Focus our element.
nodeList.focus();

// Render the screen.
screen.render();

/*
var c = new Canvas(160, 160);

function draw() {
  c.clear();
  var t = new Date();
  var sin = function(i, l) {
    return Math.floor(Math.sin(i*2*Math.PI)*l+80);
  }, cos = function(i, l) {
    return Math.floor(Math.cos(i*2*Math.PI)*l+80);
  };
  line(80, 80, sin(t.getHours()/24, 30), 160-cos(t.getHours()/24, 30), c.set.bind(c));
  line(80, 80, sin(t.getMinutes()/60, 50), 160-cos(t.getMinutes()/60, 50), c.set.bind(c));
  line(80, 80, sin(t.getSeconds()/60+(+t%1000)/60000, 75), 160-cos(t.getSeconds()/60+(+t%1000)/60000, 75), c.set.bind(c));
  process.stdout.write(c.frame());
}
*/

// setInterval(draw, 1000/24);