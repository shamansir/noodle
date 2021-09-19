## Bugs:

* `patch_state` seems not to be updated for nodes, see Render node and `Queue`;
* very often, Hydra rendering doesn't start at all;
* send last value immediately on connection;
* when dragged, nodes reset state;
* when dragged, links disappear;
* when user starts to drag node, it blinks;
* link overlaps the inlet/outlet hover area;
* Some nodes are harder to drag after adding them;
* Why sometimes Hydra output is not working;
* API: recursive references to buffers are not working, like:
    * `shape(4,0.9).diff(src(o0).scale(0.9).mask(shape(4,0.9,0.01)).rotate(0.1)).out(o0)`
    * `noise(10, 0).modulate(o0).blend(o0,0.9).out(o0)`
    * `src(o0).modulateRotate(noise(2,0),0.03).hue(0.003).layer(shape(2,0.125).luma().color(0,0,1)).out(o0)`

## Hydra Example:

* Knobs controls instead of numbers items in `Num` nodes;
* Preview sequences;
* [v]: Be able to compile expressions;
* [v]: Pretty-print Hydra structure;
* Nodes with images as sources;


## App features:

* [v] Don't show hidden inlets;
* Disconnect links when they're clicked;
* [v] Caluclate the number of links for slots and support styling the "empty" ones differently, be able to dim slot if it's not connected to anything;
* Show values on the slots;
* Value editors;
* [v] Ribbons;
* [v] Finish remove-node buttons;
* Better layouting, may be using `Flex` layout;
* [v]: Hover areas are still not perfect;
* Hot/cold inlets;

## API

* Extracting values from inlets in Definitions is quite painful, why don't provide User with API with using `Channel`/`Shape` purposes (get rid of `Hydra.Extract`)?
    * `SendToOutlet` / `SendToInlet` should also verify type;
* Node Families and Channel IDs could be parameters of toolkit etc., as types;
* `NoodleM`, which has `patch_state` as state and also provides access to inlets and outlets as a Free Monad (see `HalogenM`) — so what can be used in `effectful` processing function of the node;
    * Should it only allow getting/modifiying user patch state and getting inlet values (adapted to types using channels) or should it provide the whole set of actions such as connecting nodes, sending values to outlets etc.? Or both should be there, just as different algebras?
* `Osc` and `Render` nodes have only logic, considering above (`NoodleM`) implemented, move this logic to the nodes definitions instead; Same with `Buffer` nodes — it's better to keep component's logic inside the component and `Hydra`-related logic (like, compulation) inside the nodes;
* Find a way to make `Node`s a `Functor`, to be able to map data;
* `Hydra` as a separate purescript package?;
* Layouts:
    * [v]: Layouts are just positions bounds to components, unify searching by position, adding items etc. to them;
    * `reflow` should act like keeping existing nodes at the place and moving only those that don't fit;
    * Maybe be nodes layout in App should be one `BinPack (Location Node.Id)` where `Location a = Here a | Moved Pos a | Free | Replaced a (Location a)`, so the order of the nodes is kept and the previously reverved area in the DOM is kept even when the node was moved or removed, just translates to another place; but that could raise problems with z-index;
    * Some universal layouting like `elm-ui`, but for `purs` (`IsLayout` instance for it as well);
    * Layered and mixed layouts (pinned + bin-packed);

## UX and Style

* [v]: Different link styles;
* [v]: Different connector styles;
* Move CSS to `purescript-css`, use CSS styles constructor;
* More styles, PD style;

## Oher:

* I don't like the fact `patch_state` is passed through all the components up and down, i.e.L
    `User Node -> Patch -> App`  and then `App -> User Patch` and `App -> Patch -> User Node`
    may be there's a way to simplify it? (or see);
* [v]: Dockerfile, fix deployment;

## New nodes:

* [v]: `Out` with a buffer number;
* [v]: `to-buffer` and `from-buffer` nodes;
* Render node;
* Fast node;
* Math node;
* Pi node;
* [v] Time node;
* [v]: Mouse.x / Mouse.y nodes;
* Screen width / height nodes;
* Canvas width / height nodes;
* [~] Node for sequences — accepting values so that sequences could be joined;
* Preview node (to see the contents of the output);
* [v] The nodes with some status display inside (i.e. `osc`);
* [v] The node with interactive control inside (i.e. knob in Num node);
* Nodes to help with expressions;
* Spreads node, min/max/step;
* [v]: Color node with preview;
* [v]: Palette node;
