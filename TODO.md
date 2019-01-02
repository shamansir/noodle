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

Think on special `d` data format, like `Bang | Skip | Pass d | Decline d | Adapt (d -> d) | Error x | ...`. It could be different for node processing functions and inlet/outlet flows, or can be not. In general, it would be cool if data status would be encoded in the flow (see `Process.purs`). On the other hand, it will require API user to specify her own error type... if we don't use API errors for that. If we do use API errors, then if user data type will contain custom errors by itself, user will be required to still handle this kind of errors — starts to be complicated.

Secondary:
==========

How may user specify the types for the nodes or channels if user wants to render them differently depending on type? Should the Node/Channel renderer be stored in definition structure instead? For channels, `data` type may be the marker for a renderer, but for Node, what should be the marker. The Node, however, may store some complex type as marker (i.e. data type == `Color | String | NamedColor Color String` and so the Node, having the inputs of `Color` and `String` types, in the body operates with this `NamedColor` structure). Maybe `data` is what should be rendered and node body renderer belongs to the node? How to search for the nodes if they have no readable type? May be all the definitions should be extensible records, so user will be able to add `type` field manually and so determine the type in the renderer when some Node/Channel has its time to render? Phantom Types!? https://frigoeu.github.io/phantomtypes.html, https://stepik.org/lesson/31555/step/7?unit=11808.

Add `RPD` Effect. May be it should be produced by all these functions which subscribe to new data flows, and so the result of the `Rpd.run` function should contain it as well.

Join Paths with the same data type and make them easily extractable to arrays.

After dealing with tests, think on:

    * Returning `Aff` instead of `Eff` from any renderer or any handler, and so from `Rpd.run`: so handler would be able to handle the errors or pass them to the `main` function;
    * Add _node flow_ (i.e. `process` function) subscriptions/canceling to `Subscribers` and `Cancelers`;
    * Think on moving `Subscribers` and `Cancelers` into the _prepared_ `Network`: start the subscriptions with the same `fold` as for rendering now, but inside `run` (may be we'll have to move `Rpd.run` to `Rpd.Flow.run`);
    * ...^ so we'll be able to manage subscriptions in `connect` / `disconnect` / `addNode` etc. functions, return `Eff`s from them and so may be even deal this way with `unsafePerformEff`;
    * Since all the data flow should start/work just by running the Rpd system, without any special hander, even with the `pure unit` one;
    * On the other hand we don't need effects/subscriptions to construct new data flows in these cases — we may just use maps/sampleOn etc. to create the new flow and the renderer (or any effectful handler) should react accordingly to situation: subscribe the new flow, for example;
    * Maybe any Canceler should be `data Canceler = InletCanceler InletPath (Eff ... ) | OutletCanceler OutletPath (Eff ... ) | NodeCanceler NodePath (Eff ...)` — that would complicate searching for a proper canceler in Arrays (though we still may keep them in Maps for faster access),but that would simplify types and subscriptions in general;

Consider adding errors instead of returning `Maybe`s in some cases, with `Either` or not, I don't know. For example, if connection nwas failed, it's better to know the reason why.

IDs and Paths:

    We don't know IDs and Paths before constructing the network, since they describe the path in full, and requires the whole structure to be ready, it's not very convenient. Could that be a `Maybe` field in every definition? If it is set, then the subject is already in the network, if it's not, it's detached, but still can be accessed. Running the network fills the paths.

    May be random IDs/Hashes are ok, but so we produce `RANDOM` effect even while we construct the network. (Why not though).

    Do we need paths at all? Do we need them for normalization?

    Do we need to separate "unprepared" and "prepared" networks (some of thought on it were somewhere around here)?

Some thoughts directly from the head, don't consider them smart:

    What if unprepared Network is `do`-able and uses commands to construct a network instead of the prepared structure? And only when we run it, we convert commands to the Network we know.

        The cons here are:

        - we need indexes (unsafe) for commands of connection etc.

        The pros here are:

        - It is easy to import networks as commands;
        - We don’t need same  commands in renderers;

    On the other hand methods are the same commands written as functions, especially now, when `connect` takes indexes actually. But we need to to keep Network the main subject, since when all the methods return network, we may make it a Monad(?). And then RPD == Eff (..) Network.

    And so no way to provide API like in Collage, where addNode returns Node and all the methods related to Node operate on the Node instance?

    We may still make them so if we make Node, Patch, Inlet, Outlet etc. also `do`-able?

    The unprepared Network is the one where there’s no FRP effect, but any other effect may persist.

    So maybe RPD === Eff ( FRP | e ) Network and NetworkDef === Eff ( RANDOM ) Network or something.

    Also `connect` should require only Nodes and Outlet and Inlet indices, if we affect Patch and not Network.

Consider sending `Inlet`/`Outlet` objects to subscribers etc. instead of their paths, since it's not handy to search for inlet name in a network you have no access to...

Maybe `Behavior` from `purescript-behaviors` is the better way to store / represent the processing function? [This page](https://github.com/funkia/hareactive) explains a lot about event a.k.a. stream/behavior differences.

We have 'unprepared network' and 'prepared network' states now — which could be confusing even while we model our API not to allow interchanging these states by accident. 'Prepared network' is the network the network which was subscribed to all data flows inside and produces data. 'unprepared network' is just structure. Maybe differtiate them using separate `Network` data tags,like literally, `Prepared` and `Unprepared` or just `Network` and `NetworkDef` (like both _deferred_ and _definition_, you see?).

Maybe get rid of `DataMsg` and use data flow listeners with the help of `sampleOn` instead? So it won't be a separate stream but rather subscribers to all the inlets and outlets signals?

Pass outlet source for inlets with data signal / data flow listeners? Think on replacing `DataSource`s with one `Flow`

Fix `unsafePerformEff` with collecting the effects to be performed in folding function and executing them on `Event.subscribe`, which actually calls effects. The question is — we need Cancelers before the `subscribe` function will be triggered, to pass them as the next value to the `fold`, but they are wrapped in the effect to be performed. Is it possible to create another event with cancelers and push them from `subscribe` handler?

Some terminal renderer, like `ncurses`. Text-rendered nodes should be moveable anyway.

Try [Incremental DOM](https://pursuit.purescript.org/packages/purescript-smolder-idom/0.1.3/docs/Text.Smolder.Renderer.IncrementalDom).

Try VDOM from `use-vdom` branch. It fails, since Smolder is no more with Smolder-VDOM (Smolder-VDOM is not updated to latest Smolder).

See https://github.com/bodil/purescript-vdom/blob/master/test/Main.purs for a reference.

Think on the ways for user to implement custom node types. Are they just functions to create custom nodes?
