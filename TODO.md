Primary:
========

Tests for connecting and disconneting.

Implement processing data in the nodes. + Tests. Think on the fact that connecting nodes or even just adding an inlet to the node should produce the effect, since we should subscribe processing function to a new data source (even if it's not a function, but `Flow`). When node was added or removed, or patch structure was changed in any way, renderer should react correspondingly — trigger the updates to subscriptions etc.

Add abitity to collapse nodes by clicking their title.

Implement creating links with dragging..

Implement removing nodes.

Implement multiple selections in UI.

Remove `update'` from `Render.purs` and create visual history of events in UI, as a component.

Implement adding nodes.

Implement import / export.

Error system.

Renderers and Styles (may be from JS?).

Separate `Network` and its construction from `Rpd` core.

Make `Rpd` a structured module, not just a single file.

Maybe rename `Interactions` to `Sub`s like they are named in `Elm`.

Make `Interaction`s act similarly to subscriptions in Elm: allow to subscribe them separately (i.e. clicks, data) and fire corresponding messages in response, receiving the last model/state on each conversion, to create `Message` from `Interaction`.

`Bang` message.

Aren't `Link`s duplicate the inlet sources, may be they shoun't be stored in the `Network`, but rather be collected from it when it is required to render them.

Actually some data flow could duplicate each other.

Make Network normalized after creation (not a tree, but a collection of Patches, Nodes, Inlets, Outlets, Links arrays, may be paths may still stay as keys, may be paths or subjects could contain both global indices and nested paths).

Secondary:
==========

Add `RPD` Effect. May be it should be produced by all these functions which subscribe to new data flows, and so the result of the `Rpd.run` function should contain it as well.

Join Paths with the same data type and make them easily extractable to arrays.

After dealing with tests, think on:

    * Returning `Aff` instead of `Eff` from any renderer or any handler, and so from `Rpd.run`: so handler would be able to handle the errors or pass them to the `main` function;
    * Add _node flow_ (i.e. `process` function) subscriptions/canceling to `Subscribers` and `Cancelers`;
    * Think on moving `Subscribers` and `Cancelers` into the _prepared_ `Network`: start the subscriptions with the same `fold` as for rendering now, but inside `run` (may be we'll have to move `Rpd.run` to `Rpd.Flow.run`);
    * ...^ so we'll be able to manage subscriptions in `connect` / `disconnect` / `addNode` etc. functions, return `Eff`s from them and so may be even deal this way with `unsafePerformEff`;
    * Since all the data flow should start/work just by running the Rpd system, without any special hander, even with the `pure unit` one;
    * On the other hand we don't need effects/subscriptions to construct new data flows in these cases — we may just use maps/sampleOn etc. to create the new flow and the renderer (or any effectful handler) should react accordingly to situation: subscribe the new flow, for example;

Maybe `Behavior` from `purescript-behaviors` is the better way to store / represent the processing function?

We have 'unprepared network' and 'prepared network' states now — which could be confusing even while we model our API not to allow interchanging these states by accident. 'Prepared network' is the network the network which was subscribed to all data flows inside and produces data. 'unprepared network' is just structure. Maybe differtiate them using separate `Network` data tags,like literally, `Prepared` and `Unprepared` or just `Network` and `NetworkDef` (like both _deferred_ and _definition_, you see?).

Maybe get rid of `DataMsg` and use data flow listeners with the help of `sampleOn` instead? So it won't be a separate stream but rather subscribers to all the inlets and outlets signals?

Pass outlet source for inlets with data signal / data flow listeners? Think on replacing `DataSource`s with one `Flow`

Fix `unsafePerformEff` with collecting the effects to be performed in folding function and executing them on `Event.subscribe`, which actually calls effects. The question is — we need Cancelers before the `subscribe` function will be triggered, to pass them as the next value to the `fold`, but they are wrapped in the effect to be performed. Is it possible to create another event with cancelers and push them from `subscribe` handler?

Some terminal renderer, like `ncurses`. Text-rendered nodes should be moveable anyway.

Try [Incremental DOM](https://pursuit.purescript.org/packages/purescript-smolder-idom/0.1.3/docs/Text.Smolder.Renderer.IncrementalDom).

Try VDOM from `use-vdom` branch. It fails, since Smolder is no more with Smolder-VDOM (Smolder-VDOM is not updated to latest Smolder).

See https://github.com/bodil/purescript-vdom/blob/master/test/Main.purs for a reference.

Think on the ways for user to implement custom node types. Are they just functions to create custom nodes?
