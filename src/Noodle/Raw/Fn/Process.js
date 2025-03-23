import * as jsEnv from "browser-or-node";

console.log(jsEnv.isBrowser);

import sandboxPkg from '@nyariv/sandboxjs';

// FIXME: `Sandbox` initialization is different in browser and in Node.js
const Sandbox = jsEnv.isBrowser ? sandboxPkg : sandboxPkg.default;

const sandbox = new Sandbox();

function adaptedReceive(receive) {
    return function (inlet) {
        return receive(inlet)();
    }
}

function adaptedSend(send) {
    return function (outlet, value) {
        return send(outlet)(value)();
    }
}

function executeJs_(jsCode) {
    const evalRes = sandbox.compile(jsCode); // eval(jsCode);

    return function (receive) {
        return function (send) {
            return function() {
                evalRes({ _receive : adaptedReceive(receive), _send : adaptedSend(send) }).run();
            }
        }
    }
};


export const executeJs = executeJs_;