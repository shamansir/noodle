rotate( angle = 10, speed )

// constant rotation
osc(50).rotate( () => time%360 ).out(o0)

// modulate rotation speed
osc(10,1,1)
  .rotate( () => time%360, () => Math.sin(time*0.1)*0.05 )
  .out(o0)