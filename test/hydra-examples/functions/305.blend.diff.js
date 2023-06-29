diff( texture )

// default
osc(9,0.1,1).diff(osc(13,0.5,5)).out(o0)

osc(1,1,2)
  .diff(shape(6,1.1,0.01)
        .scale(()=>Math.sin(time)*0.05 + 0.9)
        .kaleid(15)
        .rotate(()=>time%360))
  .out()