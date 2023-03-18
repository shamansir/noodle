`GenerateToolkit.purs` CLI app could be used to generate the code for your toolkit from the brief textual definition.

To use it, you may run it with `spago`:

```sh
spago run -m Noodle.Text.GenerateToolkit --exec-args '-t hydra -d ./src/Toolkit/HydraGen -x ./hydra.v2.toolkit -m "Toolkit.HydraGen.Types as H" -l "H."'
```

To get help on the options, run either without the arguments or with `--help` argument:

```
spago run -m Noodle.Text.GenerateToolkit --exec-args '--help'
```


The line in the `.toolkit` file for the node family is:

```
tag : family :: inlets => outlets
```

optionally, the implementation could be included:

```
tag : family :: inlets => outlets /-| implementation code |-/
```

See `./hydra.toolkit` and `./hydra.v2.toolkit` for the examples.

And the parts are:

* `tag` — any string to represent the group where the node family belongs;
* `family` — the name of the node family;
* `inlets` — node inlets, could be:
    * an empty space to represent no inlets;
    * or `<>` to represent no inlets;
    * or the sequence in the format `<inlet1 -> inlet2 -> inlet3 -> ...>`, where every `inletN` could be:
        * `?` to represent an unknown value, the type holes will be placed in the generated code for it;
        * `name` some helpful name which will be used to generate proper symbol proxies and type holes as well;
        * `name:Type` to specify the type of the inlet (namespacing could be done using the `--locals-prefix` argument to the generator, none by default);
        * `name:Type {defaultValue}` to also specify the default value for the inlet (namespacing is applied here as well);
        * `name {defaultValue}` for the default value without the type;
* `outlets` — node outlets, could be:
    * an empty space to represent no outlets;
    * or `<>` to represent no outlets;
    * or `Type` to represent single outlet with the default name `out`;
    * or `Type {defaultValue}` to represent single outlet with the default name `out`;
    * or the sequence in the format `<outlet1 -> outlet2 -> outlet3 -> ...>`, where every `outletN` could be:
        * `?` to represent an unknown value, the type holes will be placed in the generated code for it;
        * `name` some helpful name which will be used to generate proper symbol proxies and type holes as well;
        * `name:Type` to specify the type of the outlet (namespacing could be done using the `--locals-prefix` argument to the generator, none by default);
        * `name:Type {defaultValue}` to also specify the default value for the outlet (namespacing is applied here as well);
        * `name {defaultValue}` for the default value without the type;
* `implementation` — any code to send from the `out` outlet in the processing function;
