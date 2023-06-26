// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Happy Mandala
// By Abhinay Khoparzi
// twitter/github/instagram: @khoparzi
voronoi(5,-0.1,5)
.add(osc(1,0,1)).kaleid(21)
.scale(1,1,2).colorama().out(o1)
src(o1).mult(src(s0).modulateRotate(o1,100), -0.5)
  .out(o0)
