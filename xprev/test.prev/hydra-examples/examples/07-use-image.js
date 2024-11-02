var myElement = document.createElement('img'); // <img />

myElement.src = '/path/to/img.jpg';    // <img src=" " />

s0.init(
  {
    src: myElement , // variable that holds element
    dynamic: false   // set true for video
  }
);

src(
  s0
)
.scrollX(0, 0.01)
.kaleid(4)
.out();
