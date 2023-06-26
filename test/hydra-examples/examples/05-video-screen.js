// use webcam as source buffer
s0.initCam();

// specify a specific camera to use
// by passing an integer
//
//    e.x. built in front facing camera
//         or using a USB webcam
//
s0.initCam(1);

// take entire current screen view
// and send to source buffer
s0.initScreen();

// take third window in array
// and send to source buffer
s0.initScreen(2);

// clear source buffer
s0.clear();
