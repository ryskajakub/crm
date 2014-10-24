var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var MachineConstants = require('../constants/MachineConstants');
var _ = require("underscore");
var storeMixin = require("../utils/storeMixin");

var CHANGE_EVENT = "change";

var machines = {};

var MachineStore = merge(storeMixin, merge(EventEmitter.prototype, {

  getCollection: function() {
    return machines;
  }

  , getByCompanyId: function(companyId) {
    return _.pick(machines, function(value, key) {
      return (value.companyId == companyId);
    });
  }

  , emitChange: function() {
    this.emit(CHANGE_EVENT);
  }

  /**
   * @param {function} callback
   */
  , addChangeListener: function(callback) {
    this.on(CHANGE_EVENT, callback);
  }

  /**
   * @param {function} callback
   */
  , removeChangeListener: function(callback) {
    this.removeListener(CHANGE_EVENT, callback);
  }
}));

function addMachines(machines_) {
  machines = machines_;
};

MachineStore.dispatchToken = AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {

    case MachineConstants.SERVER_INITIAL_MACHINES:
      addMachines(action.machines);
    break;

    default:
      return true;
  }

  MachineStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = MachineStore;
