// TODO: use safe-eval or https://github.com/nyariv/SandboxJS

import sandboxPkg from '@nyariv/sandboxjs';

const Sandbox = sandboxPkg.default;

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