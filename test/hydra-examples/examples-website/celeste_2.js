// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/

 // Puertas
// por Celeste Betancur
// https://github.com/essteban

osc(13,0,1)
  .modulate(osc(21,0.25,0))
  .modulateScale(osc(34))
  .modulateKaleid(osc(55),0.1,1)
  .out()
