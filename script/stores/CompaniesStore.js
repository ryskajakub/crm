var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');

var companies = {
	1: {
		"name": "Firma 1"
		, "days": 10
	},
	2: {
		"name": "Firma 2"
		, "days": 20
	}
};

var CompainesStore = merge(EventEmitter.prototype, {
  getAll: function() {
    return companies;
  },
});

module.exports = CompainesStore;
