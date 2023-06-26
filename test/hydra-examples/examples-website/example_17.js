// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// moire
// by Olivia Jack
// twitter: @_ojack_

pattern = () => osc(200, 0).kaleid(200).scale(1, 0.4)
//
pattern()
  .scrollX(0.1, 0.01)
  .mult(pattern())
  .out()
// https://hydra.ojack.xyz/?sketch_id=example_17