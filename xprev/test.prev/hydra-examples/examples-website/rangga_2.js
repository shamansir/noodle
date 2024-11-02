// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Monochrome Memoar
// by Rangga Purnama Aji
// https://ranggapurnamaaji1.wixsite.com/portfolio

voronoi(50,1)
  .luma(0.5).add(shape(1,1).luma(1))
  .modulate(osc(-1000,-1)
            .modulate(osc().luma()))
  .blend(o0)
  .blend(o0)
  .blend(o0)
  .blend(o0)
  .out()
