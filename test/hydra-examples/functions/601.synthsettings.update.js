update(  )

// update is called every frame
b = 0
update = () => b += 0.01 * Math.sin(time)
shape().scrollX(()=>b).out(o0)