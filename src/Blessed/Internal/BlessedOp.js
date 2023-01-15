// "use strict";

const blessed = require('blessed');
// console.log(blessed);

const registy = {};

function execute(program) {
    return function() {
        console.log(program);

        console.log(program.root);
        /*
        console.log('try execute once');
        program.value1[0].call();
        console.log('try execute twice');
        program.value1[0].call()();
        */
    }
}

function registerNode(node) {
    return function() {
        console.log(node);
    }
}

function registerHandler(handler) {
    return function() {
        console.log(handler);
    }
}

exports.execute_ = execute;
exports.registerNode_ = registerNode;
exports.registerHandler_ = registerHandler;