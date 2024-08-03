// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Puertas III
// por Celeste Betancur
// https://github.com/essteban

osc(40,0.2,1)
  .modulateScale(osc(40,0,1).kaleid(8))
  .repeat(2,4)
  .modulate(o0,0.05)
  .modulateKaleid(shape(4,0.1,1))
  .out(o0)

  // https://hydra.ojack.xyz/?sketch_id=celeste_1