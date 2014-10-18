var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');
var _ = require("underscore");

var CHANGE_EVENT = "change";

var companies = {};

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

function addCompany(company) {
  var key = _.keys(company)[0];
  companies[key] = company[key];
}

CompanyStore.dispatchToken = AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {

    case CompanyConstants.SERVER_CREATED_COMPANY:
      addCompany(action.company);
    break;

    case CompanyConstants.SERVER_INITIAL_COMPANIES:
      companies = action.companies
    break;

    default:
      return true;
  }

  CompanyStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CompanyStore;
