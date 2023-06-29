posterize( bins = 3, gamma = 0.6 )

// static gradient posterized, varying bins
gradient(0).posterize( [1, 5, 15, 30] , 0.5 ).out(o0)

// static gradient posterized, varying gamma
gradient(0).posterize( 3, [0.1, 0.5, 1.0, 2.0] ).out(o0)

// posterize (top); compare with pixelate (bottom)
osc().posterize(3,1)
  .layer(osc().pixelate(16,1)
    .mask(shape(2,0.5,0.001).scrollY(-0.25)))
  .out(o0)