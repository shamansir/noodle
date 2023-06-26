// Take a shape with 3 sides
// modify scale using
// a sin wave with `time` as
// x value

shape(3)
.scale(
 () => Math.sin(time)
)
.out()
