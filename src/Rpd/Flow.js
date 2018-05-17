"use strict";

exports.foldH = function (f) {
  return function(e) {
    return function(b) {
      var result = b;
      console.log('b', b);
      return function(sub) {
        console.log('sub', sub);
        return e(function(a) {
          console.log('a', a, 'prevResult', result);
          var fA = f(a);
          console.log('fA', fA);
          var fARes = fA(result);
          console.log('fARes', fARes);
          sub(result = fARes);
        });
      };
    };
  };
};
