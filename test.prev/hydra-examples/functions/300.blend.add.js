add( texture, amount = 1 )

// default
shape().scale(0.5).add(shape(4),[0,0.25,0.5,0.75,1]).out(o0)

osc(9,0.1,1).add(osc(13,0.5,5)).out(o0)