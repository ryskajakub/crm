var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');

var CHANGE_EVENT = "change";

var currentPage;

var setPage = function(page) {
	currentPage = {"page" : page };
}

setPage(CompanyConstants.PAGE_MAIN);

var CurrentPageStore = merge(EventEmitter.prototype, {
  get: function() {
    return currentPage;
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

		case CompanyConstants.RETURN_TO_MAIN_PAGE:
			setPage(CompanyConstants.PAGE_MAIN);
			CurrentPageStore.emitChange();
		break;

		case CompanyConstants.COMPANY_SHOW_DETAIL:
			setPage(CompanyConstants.PAGE_COMPANY_DETAIL);
			CurrentPageStore.emitChange();
		break;

    default:
      return true;
  }

  CurrentPageStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CurrentPageStore;
