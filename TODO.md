Primary:
========

Implement creating links with clicking source outlet and target inlet.

Implement creating links with dragging.

Implement disconnecting links.

Implement removing nodes.

Text-rendered nodes should be moveable anyway.

Remove `update'` from `Render.purs` and create visual history of events in UI, as a component.

Make `Rpd` a module, not just a single file.

Add multi-selection for nodes and patches.

Add abitity to collapse nodes.

Implement adding nodes.

Implement import / export.

Error system.

Renderers and Styles (may be from JS?).

Tests.

Secondary:
==========

Make Network normalized after creation (not a tree, but a collection of Patches, Nodes, Inlets, Outlets, Links arrays, may be paths may still stay as keys).

Make Patches, Nodes, Inlets, Outlets, Links to be records, this will simplify unpacking etc.

Join Paths with the same data type and make them easily extractable to arrays.

Maybe get rid of `DataMsg` and use data flow listeners with the help of `sampleOn` instead? So it won't be a separate stream but rather subscribers to all the inlets and outlets signals?

Pass outlet source for inlets with data signal / data flow listeners?

Try [Incremental DOM](https://pursuit.purescript.org/packages/purescript-smolder-idom/0.1.3/docs/Text.Smolder.Renderer.IncrementalDom).

Try VDOM from `use-vdom` branch. It fails, since Smolder is no more with Smolder-VDOM (Smolder-VDOM is not updated to latest Smolder).

See https://github.com/bodil/purescript-vdom/blob/master/test/Main.purs for a reference.
