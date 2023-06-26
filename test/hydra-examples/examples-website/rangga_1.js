// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Tag & Sweep
// by Rangga Purnama Aji
// https://ranggapurnamaaji1.wixsite.com/portfolio

osc(5,0.125).colorama(1)
  .luma(0.125).add(shape(1,0.5).luma(2).diff(gradient(1)))
  .diff(osc(-1,-0.25)).blend(o0).color(0,2.5,1.75)
  .out()
