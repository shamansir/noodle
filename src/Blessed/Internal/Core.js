// "use strict";

const blessed = require('blessed');
// console.log(blessed);

function execute(program) {
    return function() {
        console.log(program);
    }
}

exports.execute_ = execute;