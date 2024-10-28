mouse = { x, y }

shape(99).scroll(
    () => -mouse.x / width,
    () => -mouse.y / height)
    .out(o0)