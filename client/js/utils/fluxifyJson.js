var _ = require("underscore");

// takes elements in format [[id, {objectAttr: "Value"}, ...]
// and makes from it an object {id : {objectAttr: "Value"}, ...}
// that is suited for use in flux stores
var fluxifyJson = function(json) {
  return _.reduce(json, function(acc, element) {
    var id = element[0];
    var object = element[1];
    acc[id] = object;
    return acc;
  }, {});
}

module.exports = fluxifyJson;
