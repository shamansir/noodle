This is the draft implementation of _Blessed_ Node.JS library in PureScript. The library provides you as a developer with windows-driven GUI (including basic components) to the terminal applications.

There are windows a.k.a. boxes with support of dragging, lists of items, a file manager, buttons, and many more, [see for yourself](https://github.com/chjj/blessed).

What is moved:

* All the components from the basic repository;
* Most of the options, properties and methods;
* Most of the events;
* Most of the component styles;
* Tagging the text; 

Not yet translated/implemented:

* The subjects of the events handlers, so it would be easy for you to subscribe to the event of the component and get the corresponding object decoded from JSON;
* A lot of things...

It is crucial to install `npm install -g blessed` beforehand and ensure to have the latest version of node.js, or just the best one of the latest versions.

The key in this PureScript implementation is the `Key`... and `Subject` :). [Subject](https://github.com/shamansir/purescript-blessed/blob/main/src/Blessed/Internal/BlessedSubj.purs). [NodeKey](https://github.com/shamansir/purescript-blessed/blob/main/src/Blessed/Internal/NodeKey.purs). 

There are type-level values for every kind of component in _blessed_, called `Subject`, and this way the library verifies if you specify the property that really belongs to this component (or its parent) or call a method that really belongs to this component and so on.

Every component _instance_ should have an unique `Key`. Using this key you may call any method of this particular instance, that belongs to the components of this `Subject`. You may either define all of them in some separate module (e.g. `MyApp.Keys`) or create them on the fly or define them in the modules of your components. On the type-level, `Key` contains a `Subject` and some `Symbol` (type-level `String`) that identifies your family of components. On the value-level, there is a hash UUID inside so that you would be able to create many unique keys from the same family if you need. That's what `NodeKey.next` is for. If you don't have the requirement to spawn several instances of the same component in your application, then it's very probable that you won't need it.
 
There is a free monad `BlessedOp` in which you may perform any effects, access and modify the global state (there are no per-component states for the moment, at least it could require some tricks), since it implements `State` monad, and call methods of your components using keys. [BlessedOp](https://github.com/shamansir/purescript-blessed/blob/main/src/Blessed/Internal/BlessedOp.purs).

This monad is where your initial code is performed as well as the code for every handler.

Components are defined this way:

```purescript
Blessed.box <yourKey>
	[ Box.<someOption> <someValue>
	, Box.<someOption> <someValue>
	, Box.mouse true -- for example
	, Box.tags true -- for example
	, Element.<someOption> <someValue> -- since Element is parent for box
	, Blessed.on Event.Click $ do 
		 <yourKey> >~ Box.<someMethod> <arg1> <arg2>
		 val <- <yourKey> >~ Box.<someProperty> -- get value of the property
		 top <- <yourKey> >~ Box.top -- for example
		 state <- State.get
		 State.modify (\state -> <some code>)	
	     ...
	     pure unit
	]
```

Where `Blessed.box` could be `Blessed.list` or `Blessed.fileManager` etc. 

Or, this way:

```
Blessed.boxAnd <yourKey>
	[ Box.<someOption> <someValue>
	, Box.<someOption> <someValue>
	, Box.mouse true -- for example
	, Box.tags true -- for example
	, Element.<someOption> <someValue> -- since Element is parent for box
	, Blessed.on Event.Click $ do 
		 <yourKey> >~ Box.<someMethod> <arg1> <arg2>
		 val <- <yourKey> >~ Box.<someProperty> -- get value of the property
		 top <- <yourKey> >~ Box.top -- for example
		 state <- State.get
		 State.modify (\state -> <some code>)	
	     ...
	     pure unit
	]
	$ do 
		 <initial code below>
		 <yourKey> >~ Box.<someMethod> <arg1> <arg2>
		 val <- <yourKey> >~ Box.<someProperty> -- get value of the property
		 top <- <yourKey> >~ Box.top -- for example
		 state <- State.get
		 State.modify (\state -> <some code>)	
	     ...
	     pure unit		
```

Where `Blessed.boxAnd` could be `Blessed.listAnd` or `Blessed.fileManagerAnd` etc. 

`>~` is actually just the `#` operator in disguise. Just to distinguish the parts that work with _blessed_ component from other function calls.

All the components are of type `Blessed m` where `m` is usually just the `Effect` monad, or it can be something else that has instance of `MonadEffect`. [Blessed definition](https://github.com/shamansir/purescript-blessed/blob/main/src/Blessed/Internal/Core.purs#L72).

`BlessedOp state m` is the free monad for handlers and initialisation code, where `m` should be the same as above, and `state` is the global state of your components. 

To run the application, use `Blessed.run <initialState> <rootComponent>` or `Blessed.runAnd <initialState> <rootComponent> $ do ....`.

You may see many examples of its usage in the `Noodle/Cli` application, and currently this repo is just the extract from there. [Noodle CLI components](https://github.com/shamansir/noodle/tree/main/src/Cli/Components).

To tag your text so that it is coloured you may use `Blessed.Tagger`, [here's an example](https://github.com/shamansir/noodle/blob/main/src/Cli/Tagging.purs).

As the example shows, it is possible to use these bindings and they do really work in most of the cases. Still, this library is made for a specific project to satisfy its specific needs and lacks a lot of features of node.js-version of _blessed_. That's why I have no intention to publish it to Pursuit yet, at least until the events system is moved and all of the methods are implemented for basic components, and more documentation to be in the code.`
