// "use strict";

const blessed = require('blessed');
// console.log(blessed);

let registry;
let handlersFns;

const BLESSED_ON = true;
const LOG_ON = !BLESSED_ON;

function ___log() {
    if (!LOG_ON) return;
    // to be able to disable logs at once
    console.log.apply(this, arguments);
}

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
        ___log(program);
        registry = {};
        handlersFns = buildRecord(program.handlersFns, (hdl) => ({ name : hdl.index, value : hdl }));

        registerNode(program.root)();
        /*
        ___log('try execute once');
        program.value1[0].call();
        ___log('try execute twice');
        program.value1[0].call()();
        */
    }
}

function registerNode(node) {
    return function() {
        ___log(node);

        const props = buildRecord(node.props, adaptProp);
        const handlers = buildRecord(node.handlers, (evt) => ({ name : evt.event, value : evt }));
        ___log(props);
        ___log(handlers);

        let blessedObj = null;
        switch (node.kind) {
            case 'screen':
                if (!BLESSED_ON) break;
                blessedObj = blessed.screen(props);
                break;
            case 'box':
                if (!BLESSED_ON) break;
                blessedObj = blessed.box(props);
                break;
            default:
              ___log(`Unknown node kind ${node.kind}.`);
        }

        registry[node.nodeId] = { source : node, blessed : blessedObj };
        ___log('registered at', node.nodeId);

        let handlerI, handler, handlerFn;
        for (handlerI = 0; handlerI < node.handlers.length; handlerI++) { // use forEach dude
            handler = node.handlers[handlerI];
            if (handler.event === 'init') continue;
            handlerFn = handlersFns[handler.index];

            ___log('registering handler', handler, handlerFn);
            if (BLESSED_ON && handlerFn && (handlerFn.index === handler.index)) {
                if (handler.event === 'key') {
                    blessedObj.key(handler.args, (evt) => handlerFn.call(evt)());
                } else {
                    blessedObj.on(handler.event, (evt) => handlerFn.call(evt)());
                }
            }

            /*
            ___log('calling ', handler.index);
            if (handlerFn && (handlerFn.index === handler.index)) {
                handlerFn.call()();
            }
            */
        }

        let childI, child, childBlessed;
        for (childI = 0; childI < node.children.length; childI++) { // use forEach dude
            child = node.children[childI];
            childBlessed = registerNode(child)();
            if (blessedObj && childBlessed) blessedObj.append(childBlessed);
        }

        let initEventIndex, initHandlerFn;
        if (handlers[INIT_HANDLER_KEY]) {
            initEventIndex = handlers[INIT_HANDLER_KEY].index;
            initHandlerFn = handlersFns[initEventIndex];

            ___log('calling ', initEventIndex);
            if (initHandlerFn && (initHandlerFn.index === initEventIndex)) {
                initHandlerFn.call()();
            }
        }

        return blessedObj;

    }
}

function callCommand(nodeId) {
    return function(command) {
        // ___log('build', nodeId, command);
        return function() {
            ___log('call', nodeId, command);
            let returnObj = null;

            if (BLESSED_ON) {
                if (command.type === 'process') {
                    process[command.method].apply(this, command.args);
                } else {
                    const blessedObj = registry[nodeId] ? registry[nodeId].blessed : null;
                    // ___log(nodeId, registry[nodeId], blessedObj);

                    if (blessedObj) {
                        switch (command.type) {
                            case 'call':
                                blessedObj[command.method].apply(blessedObj, command.args);
                                break;
                            case 'set':
                                blessedObj[command.property] = command.value;
                                break;
                            case 'get':
                                returnObj = blessedObj[command.property];
                                break;
                            case 'process':
                                // handled earlier
                                break;
                            default:
                                break;
                        }

                    }

                }
            }

            return returnObj;
        }
    }
}

exports.execute_ = execute;
exports.registerNode_ = registerNode;
exports.callCommand_ = callCommand;