sub( texture, amount = 1 )

// default
osc().sub(osc(6)).out(o0)

// color remapping
osc(6,0,1.5).modulate(noise(3).sub(gradient()),1).out(o0)