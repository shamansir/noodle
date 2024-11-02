// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// Really Love
// by Abhinay Khoparzi
// http://khoparzi.com
osc(100,-0.01245,1).pixelate(50).kaleid(()=>(Math.sin(time/8)*9+3)).rotate(0,0.125)
.modulateRotate(shape(3).scale(()=>(Math.cos(time)*2)).rotate(0,-0.25)).diff(src(o0).brightness(0.3))
  .out()

  // https://hydra.ojack.xyz/?sketch_id=khoparzi_2