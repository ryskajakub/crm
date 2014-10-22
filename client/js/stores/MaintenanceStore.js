var AppDispatcher = require('../dispatcher/AppDispatcher');
var EventEmitter = require('events').EventEmitter;
var merge = require('react/lib/merge');
var MachineConstants = require('../constants/MachineConstants');
var _ = require("underscore");

var CHANGE_EVENT = "change";

var maintenances = {};

var storeEvents = function () {
  return {
    emitChange: function() {
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
  };
}

var MaintenanceStore = merge(storeEvents(), merge(EventEmitter.prototype, {

  get: function(id) {
    return (
      (id === undefined) ?
      maintenances :
      maintenances[id]
    );
  }

}));

function addMaintenances(maintenances_) {
  maintenances = maintenances_;
};

MaintenanceStore.dispatchToken = AppDispatcher.register(function(payload) {
  var action = payload.action;

  switch(action.type) {

    case MachineConstants.SERVER_INITIAL_MAINTENANCES:
      addMaintenances(action.maintenances);
    break;

    default:
      return true;
  }

  MaintenanceStore.emitChange();
  return true; // No errors.  Needed by promise in Dispatcher.

});


module.exports = MaintenanceStore;
