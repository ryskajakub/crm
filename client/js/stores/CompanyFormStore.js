var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');
var _ = require("underscore");

var CHANGE_EVENT = "change";

var companyNameError = "";
var companyNameAvailability = {};

var CompanyFormStore = merge(EventEmitter.prototype, {

  get: function() {
    return (
      {
        "companyNameError" : companyNameError
        , "companyNameAvailability" : companyNameAvailability
      }
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

function setCompanyNameError(newCompanyNameError) {
  companyNameError = newCompanyNameError;
}

AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {
    case CompanyConstants.SERVER_CREATE_COMPANY_FAIL:
      setCompanyNameError(action.companyNameError);
    break;

    case CompanyConstants.CHECK_AVAILABILITY:
      companyNameAvailability = action.companyNameAvailability;
    break;

    default:
      return true;
  }

  CompanyFormStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CompanyFormStore;
