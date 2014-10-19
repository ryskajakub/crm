var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var CompanyConstants = require('../constants/CompanyConstants');
var _ = require("underscore");

var CHANGE_EVENT = "change";

var companyForms = [];

var CompanyFormStore = merge(EventEmitter.prototype, {

  get: function(id) {
    if (undefined === companyForms[id]) {
      return getDefaults();
    } else {
      return companyForms[id];
    }
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

function setFieldForId(id, field, value) {
  if (undefined === companyForms[id]) {
    companyForms[id] = getDefaults();
  }
  companyForms[id][field] = value;
}

function setCompanyNameError(id, newCompanyNameError) {
  setFieldForId(id, "newCompanyNameError", newCompanyNameError);
}

function setCompanyNameAvailability(id, companyNameAvailability) {
  setFieldForId(id, "companyNameAvailability", companyNameAvailability);
}

function getDefaults() {
  return (
    {
      "companyNameError" : ""
      , "companyNameAvailability" : {}
    }
  );
}

AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {
    case CompanyConstants.SERVER_CREATE_COMPANY_FAIL:
      setCompanyNameError("new", action.companyNameError);
    break;

    case CompanyConstants.CHECK_AVAILABILITY:
      setCompanyNameAvailability("new", action.companyNameAvailability);
    break;

    default:
      return true;
  }

  CompanyFormStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = CompanyFormStore;
