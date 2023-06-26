// Declare default variable value
myVar = 100;

// Define value on key press
document.addEventListener('keydown', ()=>{
	myVar = 3 ;
});
// Return to default value after key press
document.addEventListener('keyup', ()=>{
	myVar = 100;
});

// Call variable inside of an arrow function
// to update on each frame refresh
osc(
	()=>myVar , 0.1, 0
).out();
