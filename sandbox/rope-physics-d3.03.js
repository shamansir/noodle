// CODEPEN: https://codepen.io/shamansir/pen/OPNpEab?editors=0010
// - https://cdn.jsdelivr.net/npm/d3-selection
// - https://cdn.jsdelivr.net/npm/d3-array
// - https://cdn.jsdelivr.net/npm/d3-interpolate
// - https://cdn.jsdelivr.net/npm/d3-path
// - https://cdn.jsdelivr.net/npm/d3-shape
// - https://cdn.jsdelivr.net/npm/d3-dispatch
// - https://cdn.jsdelivr.net/npm/d3-timer
// - https://cdn.jsdelivr.net/npm/d3-quadtree
// - https://cdn.jsdelivr.net/npm/d3-force


// How many simulation points a cable will have, more will take up more resources
const CABLE_SEGMENTS = 6;

// Append an SVG element
// const svg = d3.select("body").append("svg");
// svg.append('text').attr('y', 20).text('Click and drag to create a cable')

// Save reference to the last added cable
let cable;

// Draws a line out of the simulation points
const simulationNodeDrawer = d3
  .line()
  .x((d) => d.x)
  .y((d) => d.y)
  .curve(d3.curveBasis);

let i = 0;

let randomCoord = function() {
  return 150.0 + Math.random() * 750.0;
}

let startX = randomCoord;
let startY = randomCoord;
let endX = randomCoord;
let endY = randomCoord;

let startSim = function() {
    // Start a new cable
    // let c = svg.append("path").attr("stroke", d3.schemeCategory10[i % 10]);
    // cable = c;
    // cable = [];
    // Create the nodes: o  o  o  o  o
    const nodes = d3.range(CABLE_SEGMENTS).map(() => ({}));

    // Link the nodes:  o-->o-->o-->o-->o
    const links = d3
      .pairs(nodes)
      .map(([source, target]) => ({ source, target }));

    // fix the position of the first node where you clicked
    nodes[0].fx = startX();
    nodes[0].fy = startY();

    // use a force simulation to simulate the cable
    const sim = d3
      .forceSimulation(nodes)
      .force("gravity", d3.forceY(2000).strength(0.005)) // simulate gravity
      .force("collide", d3.forceCollide(20)) // simulate cable auto disentanglement (cable nodes will push each other away)
      .force("links", d3.forceLink(links).strength(0.9)) // string the cables nodes together
      .on("tick", () => {
         console.log(simulationNodeDrawer(nodes));
      }); // draw the path on each simulation tick

    // each cable has its own nodes and simulation
    // cable.datum({ nodes, sim });
    cable = { nodes, sim };
    i++;
  };

let stepSim = function() {
  if (cable) {
      const { nodes, sim } = cable;
      const start = nodes[0];
      const end = nodes[nodes.length - 1];

      // set new position of the end of the cable
      end.fx = endX();
      end.fy = endY();

      // measure distance
      const distance = Math.sqrt(
        Math.pow(end.fx - start.fx, 2) + Math.pow(end.fy - start.fy, 2)
      );

      // set the link distance
      sim.force("links").distance(distance / CABLE_SEGMENTS);
      sim.alpha(1);
      sim.restart();
    }
}

d3.select(document)
  .on("mousedown", (mouseEvent) => startSim())
  .on("mousemove", (mouseEvent) => stepSim())
  .on("mouseup", () => (cable = undefined));
