invert( amount = 1 )

// default
solid(1,1,1).invert([0,1]).out(o0)

osc(4,0.1,2).invert().luma().invert()
  .layer(osc(4,0.1,2).luma()
         .mask(shape(2,0.5).scrollY(-0.25))).out(o0)