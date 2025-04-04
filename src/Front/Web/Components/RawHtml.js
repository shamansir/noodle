"use strict";

// -- From: https://github.com/naglalakk/purescript-halogen-rawhtml

const setHTML_ = function(el) {
    return function (html) {
        return function() {
            el.innerHTML = html;
        };
    };
};

export const setHTML = setHTML_;