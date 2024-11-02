layer( texture )

// default
solid(1,0,0,1).layer(shape(4).color(0,1,0,()=>Math.sin(time*2))).out(o0)

osc(30).layer(osc(15).rotate(1).luma()).out(o0)