// TODO: use safe-eval or https://github.com/nyariv/SandboxJS

import sandboxPkg from '@nyariv/sandboxjs';

const Sandbox = sandboxPkg.default;

const sandbox = new Sandbox();

function executeJs_(jsCode) {
    const evalRes = sandbox.compile(jsCode); // eval(jsCode);

    return function (receive) {
        return function (send) {
            return function() {
                evalRes({ _receive : receive, _send : send }).run();
            }
        }
    }
};


export const executeJs = executeJs_;