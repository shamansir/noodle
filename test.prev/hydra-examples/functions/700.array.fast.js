fast( speed = 1 )

// default
osc([10,30,60].fast(2),0.1,1.5).out(o0)

// argument less than 1 makes transition slower
osc([10,30,60].fast(0.5),0.1,1.5).out(o0)