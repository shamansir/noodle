saturate( amount = 2 )

// default
osc(10,0,1).saturate( () => Math.sin(time) * 10 ).out(o0)