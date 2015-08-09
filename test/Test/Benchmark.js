'use strict';

// module Test.Benchmark

function randomArray(size) {
  return function(){
    var arr = [];
    for (var i = 0; i < size; i++) {
      arr.push(Math.random());
    }
    return arr;
  };
}

function whileUncons(predicate) {
  return function(tail){
    return function(uncons){
      return function(values){
        var tailValues = values;
        var tco = true;
        while (tco) {
          var result = uncons(tailValues);
          tco = predicate(result);
          if (tco) {
            tailValues = tail(result);
          }
        }
      };
    };
  };
}

exports.randomArray = randomArray;

exports.whileUncons = whileUncons;
