blend( texture, amount = 0.5 )

// default
shape().scale(0.5).blend(shape(4),[0,0.25,0.5,0.75,1]).out(o0)

osc(9,0.1,1).blend(osc(13,0.5,5)).out()

// motion-blur like feedback
osc().thresh().blend(o0,0.9).out(o0)