# spago run -m Noodle.Text.ParseSketch --exec-args '-i ./test/hydra-examples/examples-website/mahalia_3.js'
# spago run -m Noodle.Text.ParseSketch --exec-args '-i ./test/hydra-examples/examples/06-audio-example.js'
# spago run -m Noodle.Text.ParseSketch --exec-args '-i./test/hydra-examples/examples-website/afalfl_0.js'
# spago run -m Noodle.Text.ParseSketch --exec-args '-l ./test/hydra-examples/examples-list'
rm -Rf ./**/*.gen.purs
spago run -m Toolkit.Hydra2.Lang.ParseSketchApp --exec-args '-l ./test/hydra-examples/examples-list -o purs'