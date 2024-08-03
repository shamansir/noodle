modulate( texture, amount = 0.1 )

// chocolate whirlpool
voronoi()
  .color(0.9,0.25,0.15)
  .rotate(({time})=>(time%360)/2)
  .modulate(osc(25,0.1,0.5)
              .kaleid(50)
              .scale(({time})=>Math.sin(time*1)*0.5+1)
              .modulate(noise(0.6,0.5)),
              0.5)
  .out(o0)

// color remapping
osc(3,0,2)
  .modulate(noise().add(gradient(),-1),1)
  .out(o0)