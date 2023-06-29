modulatePixelate( texture, multiple = 10, offset = 3 )

// what lies beneath
voronoi(10,1,5).brightness(()=>Math.random()*0.15)
  .modulatePixelate(noise(25,0.5),100)
  .out(o0)

noise(3).modulatePixelate(noise(3).pixelate(8,8),1024,8)
  .out(o0)