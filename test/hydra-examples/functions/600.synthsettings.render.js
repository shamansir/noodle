render( texture = all )

// default
osc(30,0.1,1.5).out(o0)
noise().out(o1)
solid(1).out(o2)
gradient().out(o3)
render()

// specify display buffer
voronoi().out(o1)
render(o1)