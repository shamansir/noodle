We separate the `Network`, which just defines a structure for some patch network, from `RunningNetwork`, which holds this structure and _also_ manages all the effects happening inside.
Currently `UI` tries to control the flow and does subscribing and unsubscribing, by performing the effects happening, it seems to be wrong. `UI` should subscribe all the data flow once and call methods without controlling the result. Summarizing data flow should be passed to UI as event intended to be subscribed to, include inlets/outlets data, handle node processing etc.

The Flow is: all inlet and outlet data flowing through the network, including the results of processing for nodes, so renderer would be able just to subscribe to it once and forever till the application would be stopped, updating the UI properly on every change. Should be similar to Subscriptions in Elm, probably, so it will be possible to subscribe to a specific node or inlet without triggering the broadcasting for all the data.

So, user should be able both to construct the `Network` before running The Flow, and modify it (same-named methods, but with `'`?) after running it. For example, `addNode`, `connect`, `disconnect`, when applied to "static" `Network`, just modify its structure, while `addNode'`, `connect'`, `disconnect'` (they operate on `RunningNetwork`) do the same, but also produce the effect. Subscriptions are happening inside the `Rpd` system.

Even when user runs the `Rpd` system w/o any renderer, data should be sent to inlets, processing in the nodes should run, etc.

Technically it means `Subsribers` and `Cancelers` are all managed inside the `RunningNetwork`, instead of the renderer. So `RunningNetwork` produces `FRP` effect. `Renderer` receives `TheFlow` along with the `Network` instance, so it will be able to react to changes inside it. It produces `DOM` effect of course.
