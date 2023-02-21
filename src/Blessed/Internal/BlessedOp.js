// "use strict";

const blessed = require('blessed');
// console.log(blessed);

let registry;
let handlersFns;

const BLESSED_ON = false;
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
    }, {});
}

function adaptProp(hs, parentProp) {
    return function(prop) {
        switch (prop.name) {
            case 'border':
                return { name : 'border', value : buildRecord(prop.value, adaptProp(hs, 'border')) };
            case 'style':
                return { name: 'style', value : buildRecord(prop.value, adaptProp(hs, 'style')) };
            case 'item':
                return { name: 'item', value : buildRecord(prop.value, adaptProp(hs, 'item')) };
            case 'selected':
                return { name: 'selected', value : buildRecord(prop.value, adaptProp(hs, 'selected')) };
            case 'hover':
                return { name: 'hover', value : buildRecord(prop.value, adaptProp(hs, 'hover')) };
            case 'parent':
                return { name: 'parent', value : prop.tag === 'Just' ? registry[prop.value].blessed : null };
            default:
                return prop;
        }
    }
}

function execute(program) {
    return function() {
        ___log('program', program);
        registry = {};
        handlersFns = buildRecord(program.handlersFns, (hdl) => ({ name : hdl.index, value : hdl }));

        registerNode(program.root)();
    }
}

function performInit(handlerDef) {
    let initEventIndex, initHandlerFn;
    if (handlerDef) {
        initEventIndex = handlerDef.index;
        initHandlerFn = handlersFns[initEventIndex];

        ___log('calling ', initEventIndex);
        if (initHandlerFn && (initHandlerFn.index === initEventIndex)) {
            initHandlerFn.call()();
        }
    }
}

function bindHandler(blessedObj, handler) {
    if (handler.event === 'init') return;
    if (handler.event === 'command') return;
    const handlerFn = handlersFns[handler.index];

    ___log('registering handler', handler.index, handler, handlerFn);
    if (BLESSED_ON && handlerFn && (handlerFn.index === handler.index)) {
        if (handler.event === 'key') {
            blessedObj.key(handlerFn.args, (evt) => handlerFn.call(evt)());
        } else {
            blessedObj.on(handler.event, (evt) => handlerFn.call(evt)());
        }
    }
}

function registerNode(node) {
    return function() {
        ___log('node', node);

        const props = buildRecord(node.props, adaptProp(node.handlers, null));
        const handlers = buildRecord(node.handlers, (evt) => ({ name : evt.eventUniqueId, value : evt }));

        ___log('props', props);
        ___log('handlers', handlers);

        let blessedObj = null;

        switch (node.nodeSubj) {
            // FIXME just call blessed[node.nodeSubj](props)]
            case 'node':
                if (!BLESSED_ON) break;
                blessedObj = blessed.node(props);
                break;
            case 'screen':
                if (!BLESSED_ON) break;
                blessedObj = blessed.screen(props);
                break;
            case 'element':
                if (!BLESSED_ON) break;
                blessedObj = blessed.element(props);
                break;
            case 'box':
                if (!BLESSED_ON) break;
                blessedObj = blessed.box(props);
                break;
            case 'text':
                if (!BLESSED_ON) break;
                blessedObj = blessed.text(props);
                break;
            case 'line':
                if (!BLESSED_ON) break;
                blessedObj = blessed.line(props);
                break;
            case 'bigtext':
                if (!BLESSED_ON) break;
                blessedObj = blessed.bigtext(props);
                break;
            case 'list':
                if (!BLESSED_ON) break;
                blessedObj = blessed.list(props);
                break;
            case 'filemanager':
                if (!BLESSED_ON) break;
                blessedObj = blessed.filemanager(props);
                break;
            case 'listtable':
                if (!BLESSED_ON) break;
                blessedObj = blessed.listtable(props);
                break;
            case 'listbar':
                if (!BLESSED_ON) break;
                blessedObj = blessed.listbar(props);
                break;
            case 'form':
                if (!BLESSED_ON) break;
                blessedObj = blessed.form(props);
                break;
            case 'input':
                if (!BLESSED_ON) break;
                blessedObj = blessed.input(props);
                break;
            case 'textarea':
                if (!BLESSED_ON) break;
                blessedObj = blessed.textarea(props);
                break;
            case 'textbox':
                if (!BLESSED_ON) break;
                blessedObj = blessed.textbox(props);
                break;
            case 'button':
                if (!BLESSED_ON) break;
                blessedObj = blessed.button(props);
                break;
            case 'checkbox':
                if (!BLESSED_ON) break;
                blessedObj = blessed.checkbox(props);
                break;
            case 'radioset':
                if (!BLESSED_ON) break;
                blessedObj = blessed.radioset(props);
                break;
            case 'radiobutton':
                if (!BLESSED_ON) break;
                blessedObj = blessed.radiobutton(props);
                break;
            case 'prompt':
                if (!BLESSED_ON) break;
                blessedObj = blessed.prompt(props);
                break;
            case 'question':
                if (!BLESSED_ON) break;
                blessedObj = blessed.question(props);
                break;
            case 'message':
                if (!BLESSED_ON) break;
                blessedObj = blessed.message(props);
                break;
            case 'loading':
                if (!BLESSED_ON) break;
                blessedObj = blessed.loading(props);
                break;
            case 'progressbar':
                if (!BLESSED_ON) break;
                blessedObj = blessed.progressbar(props);
                break;
            case 'log':
                if (!BLESSED_ON) break;
                blessedObj = blessed.log(props);
                break;
            case 'table':
                if (!BLESSED_ON) break;
                blessedObj = blessed.table(props);
                break;
            case 'terminal':
                if (!BLESSED_ON) break;
                blessedObj = blessed.terminal(props);
                break;
            case 'image':
                if (!BLESSED_ON) break;
                blessedObj = blessed.image(props);
                break;
            case 'ansiimage':
                if (!BLESSED_ON) break;
                blessedObj = blessed.ansiimage(props);
                break;
            case 'overlayimage':
                if (!BLESSED_ON) break;
                blessedObj = blessed.overlayimage(props);
                break;
            case 'video':
                if (!BLESSED_ON) break;
                blessedObj = blessed.video(props);
                break;
            case 'layout':
                if (!BLESSED_ON) break;
                blessedObj = blessed.layout(props);
                break;

            default:
              ___log(`Unknown node kind ${node.nodeSubj}.`);
        }

        registry[node.nodeId] = { source : node, blessed : blessedObj };
        ___log('registered at', node.nodeId);

        node.handlers.forEach((handler) => {
            bindHandler(blessedObj, handler);
        });

        node.children.forEach((child) => {
            const childBlessed = registerNode(child)();
            if (blessedObj && childBlessed) blessedObj.append(childBlessed);
        });

        if (handlers[INIT_HANDLER_KEY]) {
            performInit(handlers[INIT_HANDLER_KEY]);
        }

        return blessedObj;

    }
}

function callCommand(rawNodeKey) {
    return function(command) {
        // ___log('build', nodeId, command);
        return function() {
            const nodeId = rawNodeKey.id;
            ___log('call', nodeId, command);
            let returnObj = null;

            if (BLESSED_ON) {
                if (command.type === 'process') {
                    process[command.method].apply(this, command.args);
                } else {
                    const blessedObj = registry[nodeId] ? registry[nodeId].blessed : null;

                    //console.log(blessedObj, blessedObj['selected']);
                    if (blessedObj) {
                        switch (command.type) {
                            case 'call':
                                if (command.marker == 'CallCommandEx') {
                                    blessedObj[command.method].apply(blessedObj, checkForNodes(command.args));
                                } else {
                                    blessedObj[command.method].apply(blessedObj, command.args);
                                }
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

function checkForNodes(cmdArgs) {
    return cmdArgs.map((arg) => {
        if (arg && arg['marker'] && arg['marker'] == 'Node') {
            const node = arg;
            const blessedNode = registerNode(node)();
            // TODO: check registry[node.nodeId].blessed?
            return blessedNode;
        } else {
            return arg;
        }
    });
}

function callCommandEx(rawNodeKey) {
    return function(command) {
        return function(handlers) {

            handlers.forEach((handler) => {
                handlersFns[handler.index] = handler;
            });

            return function() {
                const commandResult = callCommand(rawNodeKey)(command)();
                handlers.forEach((handler) => {
                    const blessedObj = registry[handler.nodeId] ? registry[handler.nodeId].blessed : null;
                    if (blessedObj) {
                        // console.log('registering', handler.nodeId, handler);
                        bindHandler(blessedObj, handler);
                    }
                });
                return commandResult;
            }
        }
    }
}

exports.execute_ = execute;
exports.registerNode_ = registerNode;
exports.callCommand_ = callCommand;
exports.callCommandEx_ = callCommandEx;