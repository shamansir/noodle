thresh( threshold = 0.5, tolerance = 0.04 )

// default
noise(3,0.1).thresh(0.5,0.04).out(o0)

noise(3,0.1)
  .thresh( ()=>Math.sin(time/2) , [0.04,0.25,0.75,1].fast(0.25) )
  .out(o0)

// thresh is *not* transparent; compare with luma
osc(30).layer(osc(15).rotate(1).thresh()).out(o0)