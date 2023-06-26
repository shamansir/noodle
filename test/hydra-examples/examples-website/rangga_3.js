// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Galaxy Trip
// by Rangga Purnama Aji
// https://ranggapurnamaaji1.wixsite.com/portfolio

shape(1,1)
  .mult(voronoi(1000,2)
  .blend(o0).luma())
  .add(shape(3,0.125)
       .rotate(1,1).mult(voronoi(1000,1).luma())
       .rotate(1.5)).scrollX([0.1,-0.0625,0.005,0.00001],0)
  .scrollY([0.1,-0.0625,0.005,0.00001],0)
  .out()
