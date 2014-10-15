var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');

var CHANGE_EVENT = "change";

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

var CompanyStore = merge(EventEmitter.prototype, {
  get: function(id) {
		return (
			(id === undefined) ?
			companies :
			companies[id]
		);
  },

  emitChange: function() {
    this.emit(CHANGE_EVENT);
  },

  /**
   * @param {function} callback
   */
  addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  },

  /**
   * @param {function} callback
   */
  removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }
});

AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.actionType) {

    default:
      return true;
  }

  CompanyStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CompanyStore;
