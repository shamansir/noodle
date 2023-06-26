// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
// by Zach Krall
// http://zachkrall.online/

osc(10, 0.9, 300)
.color(0.9, 0.7, 0.8)
.diff(
  osc(45, 0.3, 100)
  .color(0.9, 0.9, 0.9)
  .rotate(0.18)
  .pixelate(12)
  .kaleid()
)
.scrollX(10)
.colorama()
.luma()
.repeatX(4)
.repeatY(4)
.modulate(
  osc(1, -0.9, 300)
)
.scale(2)
.out()

// https://hydra.ojack.xyz/?sketch_id=example_11