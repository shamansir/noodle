modulateScale( texture, multiple = 1, offset = 1 )

// cosmic radiation
gradient(5).repeat(50,50).kaleid([3,5,7,9].fast(0.5))
  .modulateScale(osc(4,-0.5,0).kaleid(50).scale(0.5),15,0)
  .out(o0)

// perspective
shape(4).modulateScale(gradient().g(),2,0.5).out(o0)