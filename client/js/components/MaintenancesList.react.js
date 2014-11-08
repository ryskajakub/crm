/**
 * @jsx React.DOM
 */
var React = require('react');
var listenToStoresMixin = require("../utils/listenToStoresMixin");
var MaintenanceStore = require("../stores/MaintenanceStore");

var MaintenancesList = React.createClass({

  mixins: [listenToStoresMixin([MaintenanceStore])]

  , computeStateFromStores: function() {
    var companyId = this.props.params.companyId
    var maintenances = MaintenanceStore.getByCompanyId(companyId);
    return {"maintenances": maintenances};
  }

  , render: function () {
    var maintenances = this.state.maintenances;
    return (
      <div/>
    );
  }
});

module.exports = MaintenancesList;
