// module Flare
// jshint browser: true
// jshint node: true

"use strict";

exports.get = function (sig) {
    return function () {
      return sig.get();
    };
};
