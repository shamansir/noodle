osc( frequency = 60, sync = 0.1, offset )

// frequency
osc( [1,10,50,100,250,500].fast(2) ).out(o0)

// frequency 2
osc( () => Math.sin(time/10) * 100 ).out(o0)

// sync
osc( 10, [-10,-1,-0.1,0,0.1,1,10], 0 ).out(o0)

// offset
osc(10,0.1, ({time}) => Math.sin(time/10) * 100 ).out(o0)