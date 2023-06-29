init( options )

// load canvas
canvas = document.createElement("canvas")
canvas.width = 200
canvas.height = 200
ctx = canvas.getContext("2d")
ctx.fillStyle = "crimson"
ctx.fillRect(100,50,100,100)
s0.init({src:canvas})
src(s0).modulate(osc().kaleid(999)).out(o0)