/**
 * @jsx React.DOM
 */
var React = require('react');
var listenToStoresMixin = require("../utils/listenToStoresMixin");
var MaintenanceStore = require("../stores/MaintenanceStore");
var EmployeeStore = require("../stores/EmployeeStore");

var formatDate = require("../utils/formatDate");

var B = require("react-bootstrap");

var MaintenancesList = React.createClass({

  mixins: [listenToStoresMixin([MaintenanceStore, EmployeeStore])]

  , computeStateFromStores: function() {
    var companyId = this.props.params.companyId
    var maintenances = MaintenanceStore.getByCompanyId(companyId);
    var maintenancesWithEmployeeName = _.map(maintenances, function(maintenance) {
      var employee = EmployeeStore.get(maintenance.employeeId);
      maintenance["employeeName"] = employee["name"];
      return maintenance;
    });
    return {"maintenances": maintenancesWithEmployeeName};
  }

  , render: function () {
    var maintenances = _.map(this.state.maintenances, function (obj, key) {
      var date = formatDate(obj.date);
      var person = obj.employeeName;
      return <tr key={key}><td>{date}</td><td>{person}</td></tr>;
    });
    return (
      <B.Table striped bordered>
        <thead>
          <tr><th>Datum</th><th>Servisman</th></tr>
        </thead>
        <tbody>
          {maintenances}
        </tbody>
      </B.Table>
    );
  }
});

module.exports = MaintenancesList;
