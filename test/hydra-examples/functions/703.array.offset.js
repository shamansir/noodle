offset( offset = 0.5 )

// default
shape(999).scrollY(.2).scrollX([-0.2,0.2])
  .add(
  shape(4).scrollY(-.2).scrollX([-0.2,0.2].offset(0.5))
  ).out(o0)