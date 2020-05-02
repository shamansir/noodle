"use strict";

exports.collectPositions = function(uuids) {
    return function() {
        const vals = uuids.filter(
            function(v) { return v.kind == 'node' || v.kind == 'inlet' || v.kind == 'outlet' }
        ).map(
            function(v) {
                var selector = '[id="' + v.uuid + '"]';
                if (v.kind == 'inlet') { selector += ' .noodle-inlet-connector' };
                if (v.kind == 'outlet') { selector += ' .noodle-outlet-connector' };
                const el = document.querySelector(selector);
                const elRect = el ? el.getBoundingClientRect() : { top: -1, left: -1, bottom: -1, right: -1 };
                return {
                    kind : v.kind,
                    uuid: v.uuid,
                    pos: { x: window.scrollX + elRect.left, y: window.scrollY + elRect.top }
                };
            }
        );
        return vals;
    };
}
