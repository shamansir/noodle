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

## Existing nodes:

* Knobs controls instead of numbers items in `Num` nodes;
* Preview sequences;

## App features:

* [v]: Don't show hidden inlets;
* Disconnect links when they're clicked;
* [v]: Caluclate the number of links for slots and support styling the "empty" ones differently, be able to dim slot if it's not connected to anything;
* Show values on the slots;
* Value editors;
* Ribbons;
* [v] Finish remove-node buttons;
* Better layouting, may be using `Flex` layout;
* [v]: Hover areas are still not perfect;
* Hot/cold inlets;

## API

* Extracting values from inlets in Definitions is quite painful, why don't provide User with API with using `Channel`/`Shape` purposes?
* Node Families and Channel IDs could be parameters of toolkit etc., as types;
* Layouts are just positions bounds to components, unify searching by position, adding items etc. to them;
* Some universal layouting like `elm-ui`, but for `purs`;
* `NoodleM`, which has `patch_state` as state and also provides access to inlets and outlets as a Free Monad (see `HalogenM`) — so what can be used in `effectful` processing function of the node;
    * Should it only allow getting/modifiying user patch state and getting inlet values (adapted to types using channels) or should it provide the whole set of actions such as connecting nodes, sending values to outlets etc.? Or both should be there, just as different algebras?
* `Osc` and `Render` nodes have only logic, considering above (`NoodleM`) implemented, move this logic to the nodes definitions instead; Same with `Buffer` nodes — it's better to keep component's logic inside the component and `Hydra`-related logic (like, compulation) inside the nodes;
* Find a way to make `Node`s a `Functor`, to be able to map data;
* `Hydra` as a separate purescript package?;

## Style

* [v]: Different link styles;
* [v]: Different connector styles;
* Move CSS to `purescript-css`, use CSS styles constructor;

## Oher:

* I don't like the fact `patch_state` is passed through all the components up and down, i.e.L
    `User Node -> Patch -> App`  and then `App -> User Patch` and `App -> Patch -> User Node`
    may be there's a way to simplify it?;
* [v]: Be able to compile expressions;
* [v]: Pretty-print Hydra structure;
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
