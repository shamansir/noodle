Here we will list all the basic concepts, from the top layer to the bottom one, since Noodle could be treated as a Lasagna of concepts:

TODO picture

#Toolkit

Toolkit is just a set of node families’ definitions, where these families can be grouped, but it is not obligatory: one group can be enough.

There are no strict rules on how to select nodes for your toolkit, if you feel they belong together, then just join them!

 However, consider that when toolkits serve a single specific purpose it is easier for a user to distinguish them from each other.
 
 Examples of toolkits can be:
 
 - A set of nodes that help to generate some sketch in p5.js;
 - A set of nodes that allow to analyse and build a report for Global Finance Market;
 - A set of nodes that get data from Adruino device and allow to change its logic visually; (like Node-Red);
 - A set of nodes for building any cooking recipes where each node represent an ingridient and it’s amount in the recipe, and it is not allowed to combine anything that doesn’t match in taste or causes poisoning;
 - A set of nodes representing chemical elements that allow building complex chemical formulae;
 - A set of nodes that help operating prompts and results of LLVM (like Comfy-UI);
 - A set of nodes that give user the possibility to compose generative music (see Pure Data or Max/MSP);
 - & so on…
 
# Group

An optional grouping of families could be done in the scope of one toolkit, for example to help user to distinguish nodes of primitive mathematical purpose from nodes that draw or operate 2-dimensional shapes, from nodes that work with 3D-meshes and vertices or 3D lighting (see TouchDesigner).

#Family

A family of nodes represents all the nodes with the exact same structure of inlets, outlets, state type and processing function.

So when a user spawns an instance of `sum` node  which collects all the numerical values from its’ three inputs, sums them up and sends the resulting sum into a single output — under-the-hood that user spawns a node instance that comes from the `sum` family which defines all the nodes that have three inlets, that accept only numeric values, named `a`, `b` and `c`, all of them trigger summing function on any incoming change since they are all _hot_ inlets, all have a single numeric `out` outlet…

Internally, even though such node instance could be named `sum` following to its family, in the network it is identified by a randomly-generated hash, unique to each instance in this network. You can observe the part of this hash in the status line in the interface when you hover a mouse over a node.

TODO picture

In general, families are definitions of how instances of the nodes should operate in their own context. 

# Network

Networks consist of all the patches in the current session, it allows to create or remove patches (using the same toolkit) inside it, as well as move/copy parts of patches between each other, but doesn’t have any shared state between these patches.

#Patch

Patch is an infinite canvas (or infinite working table) where user may spawn, and connect, and operate in any possible way, nodes of different families from a single toolkit. It has its own state that is accessible to the all the spawned nodes. 

Patch can also have some drawing target on its background, behind the nodes, which can represent what nodes construction is aiming to achieve in the current state. For example, if toolkit allows composing Processing sketches, it could be the current state of the sketch in the work. 

# Node

Node is a minimal block in the visual programming concept. 

We think the best analogue for the node is a function in textual programming, but even much closer example is a function in _pure functional_ programming, where it is a strict rule of always producing the same value in the output(s) for the same values in inputs, _except_ for the cases when it also produces a side-effect.

But yes, while in both imperative and functional programming function can have maximum of one output, the `return` value (here we will close our eyes on the nuances of this not very correct statement regarding FP), nodes in Noodle, on the other hand, can have none or many outputs, however you can  just can be represented as a single value containing a record with named fields. 

Another similarity with FP is that all the inputs and outputs of a node should have their types specified in the definition (by a programmer for a user) as well as function/node inner code (processing function, see below) should comply and conform all these types.

TODO picture

The difference is the visual and interactivity part: nodes are _tangible_: they may be connected to other nodes in any of the incoming point (such as inlet), or outgoing point (such as outlet), they also can have a visual body where the result of the function call is displayed, and/or processing can be tuned with controllers. 

## Channels: Inlets & Outlets

The integral part of a node is its channels: inlets and outlets (or just inputs and outputs, respectively). They look similar to each other, but direction of the data flow is what distinguishes them. Data comes inside through one or several inlets, gets processed, and then goes further away through the outlets, to the inlets of the other nodes, if they are connected, or just stays on the outer edge if there are no outgoing connections. 

TODO picture

As  noted above, the processing function inside the node could produce _effects_, e.g. some random-number producing node, so it’s not always can be guaranteed that the same inputs values always convert to the same outputs, but often it is the case.

Because data goes inside through inlets, they have some more parameters to control on how they receive data:

### Temperature

Temperature of the node’s inlets is what decides when its processing is triggered in the moment when new data has arrived on some of them:

* If all inlets are hot (and all the values are accepted), processing fuction is triggered on any change in them right away;
* If some of these inlets are cold, the new data is received there but the latest received value is kept inside until one of the hot inlets would trigger the processing, no matter if it could never happen;
* If all the inlets of some node happened to be cold, no processing ever happens in such node;

In general, there are two distinct types of temperature rules used for toolkits:

* All the inlets of the nodes are cold;
* The first inlet if any node is hot, other ones are cold;

Usually it depends on the subject of the toolkit, keeping the first inlet hold while others cold is more common to the toolkits that generate music, where first inlet usually receives the audio signal.

### Value Editors

When a user clicks on some inlet, there’s a chance that it is possible to send a custom value to this inlet, if the type of the data that flows inside it can be represented by a string or tuned using some supported control. 

The control that appears is called _Value Editor_.

If user somehow entered an incorrect data (but it’s muxh bettet for the editor to prevent such cases using its own UX) it could not be accepted and so either value of the inlet will be erased and it will wait until some correct value will be sent there, or the value will be declined and to revive the inlet it would be required (as well) just to send new value there or, if the inlet has default fallback value, it could be used as a temporary replacement not to break the flow of the node.

There’s a chance that outlets could also have value editors on them at some point.

## Body
## Function
## Processing

Usual processing function of the node is collecting data from all or some inlets, does some inner calculations (or any other magic), and sends data to its outlets. In some cases, nodes don’t have any inlets or any outlets or both, so they collect all the data inside and manage it by themselves or, opposite, give  everything away, or are totally independent.

Since we support streams or signals of data, it means the data could flow very fast through the connection, like electricity does through the wires, sometimes processing function could operate with such flow and merge them or filter them or pass them as they are.

# Link

Link is what connects some outlet to some inlet. It makes all the data from this outlet to be transferred to the connected inlet. Inlet cam decline some data, though, becase it doesn’t conform the expected type or by any other reason.

TODO picture

For the moment in Noodle no recursive connections are possible, so it the outlet and the inlet should be from different nodes and also does not send anything back in the flow; 

Also, currently you can have only one link incoming  at some specific inlet, so when you connect the same outle and inlet again, the previous link is cancelled and new one is created, as well as on connection to some inlet that already has some link,  the previous one gets disconnected in favor of the new one;

# Repr
# Signal