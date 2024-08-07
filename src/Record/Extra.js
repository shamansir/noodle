"use strict";

const pickFn_ = function(ks, r) {
  var copy = {};
  for(var i = 0; i < ks.length; i++) {
      copy[ks[i]] = r[ks[i]];
  }
  return copy;
};

export const pickFn = pickFn_;
