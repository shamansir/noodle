// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Dreamy Diamond
// by Rangga Purnama Aji
// https://ranggapurnamaaji1.wixsite.com/portfolio

osc(7,-0.125).modulate(voronoi(1)).diff(voronoi(1).mult(gradient(-1).luma(0.125)))
  .luma(0.125)
  .add(shape(7, 0.5)
       .mult(voronoi(10,2).blend(o0).diff(gradient(1)).modulate(voronoi())))
  .scrollY(-0.1)
  .scrollX(0.125)
  .blend(o0)
  .blend(o0)
  .out()
