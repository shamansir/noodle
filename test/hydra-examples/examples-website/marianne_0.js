// licensed with CC BY-NC-SA 4.0 https://creativecommons.org/licenses/by-nc-sa/4.0/
//port
//by Marianne Teixido
//https://marianneteixido.github.io/

osc(5, 0.9, 0.001)
    .kaleid([3,4,5,7,8,9,10].fast(0.1))
    .color(0.5, 0.3)
    .colorama(0.4)
    .rotate(0.009,()=>Math.sin(time)* -0.001 )
    .modulateRotate(o0,()=>Math.sin(time) * 0.003)
    .modulate(o0, 0.9)
    .scale(0.9)
    .out(o0)
