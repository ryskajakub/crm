var Api = require("./api.js");
var api = new Api("/api");
api.Dogs.list(function(s) {console.log(s)});
