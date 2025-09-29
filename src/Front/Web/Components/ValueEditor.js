"use strict";

// -- From: https://github.com/naglalakk/purescript-halogen-rawhtml

const focus_ = function (el) {
  return function () {
    el.focus();
  };
};

export const focus = focus_;
