mult( texture, amount = 1 )

// default
osc(9,0.1,2).mult(osc(13,0.5,5)).out()

// mult is *not* transparent; compare with mask
osc()
  .layer(osc(30,0.1,2).mult(shape(4)))
  .out(o0)