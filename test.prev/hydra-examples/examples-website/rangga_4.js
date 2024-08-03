// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Sumet
// by Rangga Purnama Aji
// https://ranggapurnamaaji1.wixsite.com/portfolio

osc(0.5,1.25).mult(shape(1,0.09).rotate(1.5))
  .diff(gradient())
  .add(shape(2,2).blend(gradient(1)))
  .modulate(noise()
            .modulate(noise().scrollY(1,0.0625)))
  .blend(o0)
  .color(1,-0.5,-0.75)
  .out()
