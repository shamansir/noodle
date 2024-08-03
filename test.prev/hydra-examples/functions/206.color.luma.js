luma( threshold = 0.5, tolerance = 0.1 )

// default
osc(10,0,1).luma(0.5,0.1).out(o0)

osc(10,0,[0,0.5,1,2]).luma([0.1,0.25,0.75,1].fast(0.25),0.1).out(o0)

// luma is transparent; compare with thresh
osc(30).layer(osc(15).rotate(1).luma()).out(o0)