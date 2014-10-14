var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');

var CHANGE_EVENT = "change";

var company = null;

var CompanyDetailStore = merge(EventEmitter.prototype, {
  get: function() {
    return {"company": company};
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

		case CompanyConstants.COMPANY_SHOW_DETAIL:
			company = action.companyId;
		break;

    default:
      return true;
  }

  CompanyDetailStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CompanyDetailStore;
