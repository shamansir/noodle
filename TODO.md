Primary:
========

Implement better UI, looking closer to what we have in the original Rpd, so:

    - No more collapsing nodes this way, nodes are always expanded in current patch by default;
    - Selections, single by default, multiple with modifier; And it is selection, not collapse/expand;
    - Better grid for nodes, outlets should be on the right side and inlets on the left;
    - Implement Interactions, these are Clicks, Moves and User-forced commands,
      they are easily translated to Messages being Select (Modifier) / Expand / OpendPatch etc.;

Add abitity to collapse nodes by clicking their title.

Implement creating links with clicking source outlet and target inlet.

Implement creating links with dragging.

Implement disconnecting links.

Implement removing nodes.

Remove `update'` from `Render.purs` and create visual history of events in UI, as a component.

Make `Interaction`s act similarly to subscriptions in Elm: allow to subscribe them separately (i.e. clicks, data) and fire corresponding messages in response, receiving the last model/state on each conversion, to create `Message` from `Interaction`.

`Interactions` are actually `Event`s, but that will make them confused with FRP `Events`. We may either rename `Event`s from `FRP` to `Flow`s, or rename `Interactions` to `Sub`s like they are named in `Elm`.

Separate `Network` and its construction from `Rpd` core.

Make `Rpd` a module, not just a single file.

`Bang` message.

Implement adding nodes.

Implement import / export.

Error system.

Renderers and Styles (may be from JS?).

Some terminal renderer, like `ncurses`. Text-rendered nodes should be moveable anyway.

Tests.

Aren't `Link`s duplicate the inlet sources, may be they shoun't be stored in the `Network`, but rather be collected from it when it is required to render them.

Secondary:
==========

Make Network normalized after creation (not a tree, but a collection of Patches, Nodes, Inlets, Outlets, Links arrays, may be paths may still stay as keys, may be paths or subjects could contain both global indices and nested paths).

Make Patches, Nodes, Inlets, Outlets, Links to be records, this will simplify unpacking etc.

Join Paths with the same data type and make them easily extractable to arrays.

Maybe get rid of `DataMsg` and use data flow listeners with the help of `sampleOn` instead? So it won't be a separate stream but rather subscribers to all the inlets and outlets signals?

Pass outlet source for inlets with data signal / data flow listeners?

Try [Incremental DOM](https://pursuit.purescript.org/packages/purescript-smolder-idom/0.1.3/docs/Text.Smolder.Renderer.IncrementalDom).

Try VDOM from `use-vdom` branch. It fails, since Smolder is no more with Smolder-VDOM (Smolder-VDOM is not updated to latest Smolder).

See https://github.com/bodil/purescript-vdom/blob/master/test/Main.purs for a reference.

Think on the ways for user to implement custom node types. Are they just functions to create custom nodes?
