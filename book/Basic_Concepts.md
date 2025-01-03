Here we will list all the basic concepts, from the bottom layer to the top one, since Noodle could be treated as a Lasagna of concepts:

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

We think the best analogue for the node is a function in textual programming, but even much closer example is a function in pure functional programming, where it is a strict rule of always producing the same value in the output(s) for the same values in inputs, except for the cases when it produces a side-effect.

But yes, in both imperative and functional programming function can have maximum of one output, the `return` value (here we will close our eyes on the nuances of this not very correct statement regarding FP). Node, on the other hand, can have none or many outputs, however it just can be represented as a single value containing a record with named fields. 

Another similarity with FP is that all the inputs and outputs of a node should have their types specified in the definition (by a programmer for a user) as well as function/node inner code (processing function, see below) should comply and conform all these types.

TODO picture

The difference is the visual and interactivity part: nodes are _tangible_: they may be connected to other nodes in any of the incoming point (such as inlet), or outgoing point (such as outlet), they also can have a body where the result of the function call is displayed and/or can be tuned with controllers. 

## Channels: Inlets & Outlets

### Temperature
### Editors
## Body
## Function
## Processing

# Link
# Repr
# Signal
