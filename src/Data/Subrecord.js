"use strict";

exports.passNullContext = function(f) {
  return f(undefined);
};