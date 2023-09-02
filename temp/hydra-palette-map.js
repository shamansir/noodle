var DD=0.1 // 0.3

var b=
    (what,url,i,y,z)=>
	{
      return what()
          .add(solid(1,1,1),DD)
          .thresh(i*0.2*(z-y)+y,0.0001)
          .luma(0.5,0.0001)
          .color(c(url,i,0),c(url,i,1),c(url,i,2));
    }

var c=(url,i,j)=>{
  let cc = url.split("/"), cs = cc[cc.length - 1].split("-")
  return parseInt("0x" + cs[i].substring(2*j, 2+2*j))/255
}

var colorize=(what,url,y=0,z=1)=>
    {
      return b(what,url,0,y,z)
         .layer(b(what,url,1,y,z))
         .layer(b(what,url,2,y,z))
         .layer(b(what,url,3,y,z))
         .layer(b(what,url,4,y,z))
    }

var url='https://coolors.co/bbdef0-f08700-f49f0a-efca08-00a6a6'
var func=()=>noise()
colorize(func,url).out()