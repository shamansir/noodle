Primary:
========

Move to (`spago`)[https://github.com/spacchetti/spago].

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

Some terminal renderer, like `ncurses`. Text-rendered nodes should be moveable anyway. See `blessed`

Try [Incremental DOM](https://pursuit.purescript.org/packages/purescript-smolder-idom/0.1.3/docs/Text.Smolder.Renderer.IncrementalDom).

Try VDOM from `use-vdom` branch. It fails, since Smolder is no more with Smolder-VDOM (Smolder-VDOM is not updated to latest Smolder).

See https://github.com/bodil/purescript-vdom/blob/master/test/Main.purs for a reference.

Think on the ways for user to implement custom node types. Are they just functions to create custom nodes?

If we introduce GUID-paths, we either need to return them to the user on every entity creation so that this user will be able to address the newly created entities (i.e. nodes), or we should store the Num-Path /-> GUID map inside the Network, and update it on every structure change. Another way (since with `addNode`/`addInlet`/etc. methods we should both modify the `Network` _and_ return the GUID, if we want user to know it): always keep the `Network` inside the `Rpd` monad (using `StateT` or continuation monad?) and still let user get the GUID with `do` like:

```purescript
buildNetwork = do
    -- network is empty here
    nodePath <- addNode _ _ -- no network would be needed here
    inletPath <- addInlet nodePath _ _
    -- some code
    pure unit
```

See [_Implicit Lifting_](https://stepik.org/lesson/38581/step/2?unit=20506) on Stepik.


All the `subscribe___` functions returning cancellers to the user should have the default implementation which stores those cancelers inside the `Network` and calls them when the corresponding entity (Node/Inlet/etc.) is removed —— _that's covered with tests and partly done_.


Now every Inlet and Outlet has its "flow" and its own "push" function — to send the value in. While it is pretty clear what to do with inlets on processing: what flows in from the inlets connected to the node also goes directly into the `process` handler and actually triggers it, but with outlets, it's not that clear — their values may both be produced by the `process` function and as well may be sent by the user who uses their own `push` function (if we allow it, of course — should we?). Should the outlet flow contain both? Should the node contain the different `outlets` flow, which is a sum of `process` results and direct values? —— _I decided to restrict pushing to outlets since node processing should be the only source of their data_.



`data FlowMsg = Bang | Skip | Pass v | Decline v | Error x ...`

```purescript
data Incoming x d
    = Identified x d
    | Unidentified d

data Outgoing x d
    = Send x d
    | Skip
```


```purescript
-- TODO: may be find better ways to process these things in future
--       I'd like to have something similar to JS-world
--       function (inlets) { return { 'c': inlets.a + inlets.b } }
-- variants:
--  `Record.set` / `Record.get` etc.
--  `Foreign.Object`` : https://github.com/purescript/purescript-foreign-object/blob/master/src/Foreign/Object.purs
--  `liftA2 (+) (m^.at a) (m^.at b)` -- Map -> Map

-- may be ProcessF should also receive previous value
-- TODO: add Process Behavior (a.k.a. event with function) it would be possible
--       to subscribe/know outlets changes as well
-- TODO: generalize Process function to receiving something from incoming data stream and
--       sending something to outgoing data stream, so all the types of `ProcessF`` could
--       implement the same type. The question is — we need folds to store previous values,
--       will we be able to apply them with this implementation?
-- TODO: also there can be a `Pipe`d or `Direct` approach, i.e. a function
--       of type (String -> d -> (String /\ d)), where there is no need in other inlet
--       values except one, so it is called for each inlets one by one and so collects
--       values for outputs

-- data ProcessF d
--     = ByLabel (Flow (String /\ d) -> PushF (String /\ d) -> Effect Unit)
--     | ByPath (Flow (InletPath /\ d) -> PushF (OutletPath /\ d) -> Effect Unit)
    -- | Full (Flow (InletPath /\ InletDef d /\ d) -> PushF (OutletPath /\ d) -> Effect Unit)

    -- TODO: Effectful ProcessF
    -- TODO: Other types


-- TODO: some "data flow" typeclass which provides functions like:
-- `receive inletIndex -> Rpd/Effect d`,
-- `send outletIndex data -> Rpd/Effect Unit`,
-- `receive' inletLabel -> Rpd/Effect d`,
-- `send' outletLabel data -> Rpd/Effect Unit`,
-- and maybe... the `Rpd d`, `Network (Node d)` or the `Node d` should implement it,
-- for the `Node` case — it can use `_nodeInlet'`/`_nodeOutlet'` lensed and so
-- search only for the inlets inside, by label

-- data DataSource d
--     = UserSource (Flow d)
--     | OutletSource OutletPath (Flow d)
```

Use Free Monads for the different command interpreters or even RPD API itself, see Haskell Notes for Professionals: Chapter 8.


We can ask the data argument `a` in `Rpd a` to implement some typeclass like `IsData a` (`MayFlow a`) and to have `accept` and `adapt` methods there. Or, since `accept` should also get the type of a channel to compare data items with allowance rules, `IsData c a` where `c` is the channel type. Also we may have `IsRenderableData a` typeclass (`Renders a`) which contains the functions to convert data to the format of the renderer's `view`.

At first, we may get the `type` string from `InletDef`/`OutletDef` to determine the inlet's/outlet's type.

If there `Rpd d c` exists, where `d` — is the data type and `c` is the channel type, then: `accept :: c -> d -> Bool`.

The `IsData d` may only exist for the functions which require it, like `addInlet`, `addOutlet`, `connect` (just `connect`?).

Or, the best option is:  `connect :: IsChannel c d => c -> Outlet d -> Inlet d -> ...`.


Spreads??? Enum typeclass? Monoid? Anything what is implemented by `List`? Just `List` itself? Though it doesn't fit matrices/tables then. zipWith etc. As the type class? Free implementation, like Lazy Lists? Shoud it be in the standart library or is a way to pack/prepare `data` for `Rpd data`.


Rendering:

For the every data package between outlet and inlet, and any message, the `update` is triggered (not `RAF`) — then we would be able to control all the unsubscriptions in one application cycle. If `RAF` comes in, then we need to, like for the first examples, store the `Map` containing the latest data package came through this particular outlet/intlet, and render this exact package.


Maybe, just maybe, ensure that all the methods which are not doing any side-effects are not forced by `Rpd d` to be `Eff / Aff` thanks to the `ExceptT _ Effect _`. Maybe `</>` uses `pure/lift` for those functions in chaining?


Create the `Alias x` type, let it be `Alias String` at first. It will serve as the manually created inlet/oultet ID _inside_ the node, _not_ the part of the `Inlet`/`Outlet` instance. Gets received by `addInlet`/`addOutlet`. Then, every node should be able to introduce the lenses/functions such as `Alias -> Maybe Inlet` and `Alias -> Maybe Outlet` to the processing function, which guarantee the uniqueness of the inlet/outlet inside this node using the alias. Could be split in two: `IAlias`/`OAlias`. Processing functions gets `Map Alias data` with inlets data and returns the processed data as the similar `Map Alias data`, but for outlets....

Or just gets the `Alias -> data` function and returns it??!! The one we got lets getting the current (latest) value from the inlet, the one which is returned gives back the requested/calculated outlet value.

It is useful since this function could be `Map.lookup` as well, but it seems to be more generic. And so users may use pattern-matching in theirs `process` handlers (or even `do`-notation?), e.g.:

```
getInletVal "foo" = 2
getInletVal "bar" = 4
produceOutletVal "out" = 6
```

We could start with replacing `Path` with `Alias`.

And then use the same mechanics to guarantee "uniqueness" for the node in the patch. This uniqueness is guaranteed by the API user though, not us, and so the user is responsible. We may replace the paths with the chains of aliases, like as in file system. I'd like to have friendly path and still have unique hash somewhere inside.


Since `Event` a.k.a. `Flow` implements different typeclasees, we may use the similarities to implement `IsData`, `Spread` etc. Section 29, Reactive Banana.


Toolkits we cool in JS-RPD, like `anm/player` & s.o. Do we need `ToolkitName -> NodeTitle -> NodeDef` function for that? `IsToolkit` typeclass?


Allow user's `update` functions in renderers to be Effectful
