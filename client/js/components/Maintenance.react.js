/**
 * @jsx React.DOM
 */
var React = require('react');

var Moment = require("../utils/Moment");
var listenToStoresMixin = require("../utils/listenToStoresMixin");

var Machine = require("./Machine.react");
var MaintenanceForm = require("./MaintenanceForm.react");
var MachineStore = require("../stores/MachineStore");
var CompanyStore = require("../stores/CompanyStore");
var MaintenanceStore = require("../stores/MaintenanceStore");

var B = require("react-bootstrap");
var Row = B.Row;
var Grid = B.Grid;

var _ = require("underscore");

var Maintenance = React.createClass({

  mixins: [listenToStoresMixin([MachineStore, MaintenanceStore, CompanyStore])]

  , computeStateFromStores: function () {
    var maintenanceId = this.props.params.maintenanceId;
    var maintenance = MaintenanceStore.get(maintenanceId) || {};
    var company = CompanyStore.get(this.props.params.companyId);
    var machines = MachineStore.getByCompanyId(this.props.params.companyId);
    return ({
      "maintenance": maintenance
      , "company": company
      , "machines": machines
    });
  }

  , render: function() {

    var maintenance = this.state["maintenance"];
    var company = this.state["company"];

    var machines = _.reduce(this.state.machines, function (acc, value, key) {
      var selected = _.contains(maintenance["machinesIds"], parseInt(key));
      var element = <Machine key={key} machine={value} selected={selected} />
      acc.push(element);
      return acc;
    }, []);

    return (
      <main>
        <h1>Servis</h1>
        <h2>{company["name"]}</h2>
        <Grid>
          <Row>{machines}</Row>
          <Row>
            <MaintenanceForm maintenanceId={this.props.params.maintenanceId} />
          </Row>
        </Grid>
      </main>
    );
  }

});

module.exports = Maintenance;
