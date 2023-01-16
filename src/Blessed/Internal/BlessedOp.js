// "use strict";

const blessed = require('blessed');
// console.log(blessed);

let registry;
let handlersFns;

const INIT_HANDLER_KEY = 'init';

function buildRecord(array, fn) {
    return array.reduce((record, item, idx) => {
        const pair = fn(item, idx);
        record[pair.name] = pair.value;
        return record;
    }, {})
}

function adaptProp(prop) {
    switch (prop.name) {
        case 'border':
            return { name : 'border', value : buildRecord(prop.value, adaptProp) };
        case 'style':
            return { name: 'style', value : buildRecord(prop.value, adaptProp) };
        case 'hover':
            return { name: 'hover', value : buildRecord(prop.value, adaptProp) };
        default:
            return prop;
    }
}

function execute(program) {
    return function() {
        console.log(program);
        registry = {};
        handlersFns = buildRecord(program.handlersFns, (hdl) => ({ name : hdl.index, value : hdl }));

        registerNode(program.root)();
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
        registry[node.nodeId] = node;

        const props = buildRecord(node.props, adaptProp);
        const handlers = buildRecord(node.handlers, (evt) => ({ name : evt.event, value : evt }));
        console.log(props);
        console.log(handlers);

        /*
        switch (node.kind) {
            case 'screen':
                blessed.screen(props);
              break;
            case 'box':
                blessed.box(props);
            default:
              console.log(`Unknown node kind ${node.kind}.`);
        }
        */

        let initEventIndex, initHandlerFn;
        if (handlers[INIT_HANDLER_KEY]) {
            initEventIndex = handlers[INIT_HANDLER_KEY].index;
            initHandlerFn = handlersFns[initEventIndex];

            console.log('calling ', initEventIndex);
            if (initHandlerFn && (initHandlerFn.index === initEventIndex)) {
                initHandlerFn.call()();
            }
        }

        let handlerI, handler, handlerFn;
        for (handlerI = 0; handlerI < node.handlers.length; handlerI++) { // use forEach dude
            handler = node.handlers[handlerI];
            if (handler.event === 'init') continue;
            handlerFn = handlersFns[handler.index];

            console.log('calling ', handler.index);
            if (handlerFn && (handlerFn.index === handler.index)) {
                handlerFn.call()();
            }
        }

        let childI, child;
        for (childI = 0; childI < node.children.length; childI++) { // use forEach dude
            child = node.children[childI];
            registerNode(child)();
        }

    }
}

function registerHandler(handler) {
    return function() {
        console.log(handler);
    }
}

function callCommand(nodeId) {
    return function(command) {
        console.log('build', nodeId, command);
        return function() {
            console.log('call', nodeId, command);
            return { foo : "test" };
        }
    }
}

exports.execute_ = execute;
exports.registerNode_ = registerNode;
exports.registerHandler_ = registerHandler;
exports.callCommand_ = callCommand;