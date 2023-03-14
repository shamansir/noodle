rm -Rf ./src/Toolkit/HydraGen
spago run -m Noodle.Text.GenerateToolkit --exec-args '-t hydra -d ./src/Toolkit/HydraGen -x ./hydra.toolkit -m "Toolkit.HydraGen.Types as H" -l "H."'