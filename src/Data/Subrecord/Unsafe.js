"use strict";

// from: https://github.com/rubenpieters/purescript-subrecord

exports.unsafeGetFn = function(just, nothing, label, rec) {
  if ({}.hasOwnProperty.call(rec, label)) {
    return just(rec[label]);
  } else {
    return nothing;
  }
};

exports.unsafeSetFn = function(label, value, rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  copy[label] = value;
  return copy;
};

exports.unsafeDeleteFn = function(label, rec) {
  var copy = {};
  for (var key in rec) {
    if (key !== label && {}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

exports.unsafeHasFn = function(label, rec) {
  return {}.hasOwnProperty.call(rec, label);
};