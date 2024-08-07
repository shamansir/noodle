function approximateLineSegments(svg, style) {
    //origin of axes
    var origin = {
        x: -Math.PI,
        y: 0
    };

    var amplitude = 1; // wave amplitude
    var rarity = 0.5; // point spacing
    var freq = 0.5; // angular frequency
    var phase = Math.PI*2; // phase angle

    var group = document.createElementNS("http://www.w3.org/2000/svg", "g");
    group.setAttribute('style', style);

    for (var i = -30; i < 40; i++) {
        var line = document.createElementNS("http://www.w3.org/2000/svg", "line");

        line.setAttribute('x1', (i - 1) * rarity + origin.x);
        line.setAttribute('y1', Math.sin(freq * (i - 1 + phase)) * amplitude + origin.y);

        line.setAttribute('x2', i * rarity + origin.x);
        line.setAttribute('y2', Math.sin(freq * (i + phase)) * amplitude + origin.y);

        group.appendChild(line);
    }

    svg.appendChild(group);
}

function approximateCubicBezier(svg, controls, style) {
    var path = document.createElementNS("http://www.w3.org/2000/svg", "path"),
        data;

    //Bezier control points:
    //https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths#Curve_commands
    var controlStart = controls[0],
        control1     = controls[1],
        control2     = controls[2],
        controlEnd   = controls[3],
        x, y,
    	x1, y1,
    	x2, y2,
        quarterX = controlEnd[0],
        startX = -(4 * quarterX),
        negateY = false;

    function negateYs() {
        if(negateY) { y = -y; y1 = -y1; y2 = -y2; }
    }

    for(x = startX; x<6;) {
        if(x === startX) {
            y = controlStart[1];
            x1 = x + control1[0];
            y1 = control1[1];

            negateYs();
            data = 'M' +[x,y]+ ' C' +[x1,y1]+ ' ';
        }
        else {
            //x1/y1 are always "mirrors" of the previous x2/y2,
            //so we can use the simpler "S" syntax instead of a new "C":
            data += ' S'
        }

        //Going from y=0 to y=+-1:
        x2 = x + control2[0];
        y2 = control2[1];
        x += quarterX;
        y = controlEnd[1];
        negateYs();
        data += [x2,y2] + ' ' + [x,y];

        //Going from y=+- back to y=0:
        x2 = (x + quarterX) - control1[0];
        y2 = control1[1];
        x += quarterX;
        y = controlStart[1];
        negateYs();
        data += ' S' + [x2,y2] + ' ' + [x,y];

        negateY = !negateY;
    }

    console.log(data);

    //path.setAttribute('d', 'M0,0 C0.5123,0.5123 1.0023,1 1.5708,1 S2.5,0.5 3.14,0');
    path.setAttribute('d', data);
    path.setAttribute('style', style);
    svg.appendChild(path);
}


//Draw some sine waves:
var sinesGroup = document.querySelector('#sines');

approximateLineSegments(sinesGroup, 'stroke:blue;');

var controlsGood = [[0,         0],
                    [0.5,       0.5],
                    [1,         1],
                    [Math.PI/2, 1]];
var controlsBetter = [[0,                    0],
                      [0.512286623256592433, 0.512286623256592433],
                      [1.002313685767898599, 1],
                      [Math.PI/2,            1]];
approximateCubicBezier(sinesGroup, controlsGood, 'stroke:red;');
approximateCubicBezier(sinesGroup, controlsBetter, 'stroke:lime;stroke-dasharray:.5,.1');
