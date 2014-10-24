/**
 * @jsx React.DOM
 */
var React = require('react');

var _ = require("underscore");
var Moment = require("../utils/Moment");

var Router = require('react-router');
var B = require("react-bootstrap");

var listenToStoresMixin = require("../utils/listenToStoresMixin");
var MachineStore = require("../stores/MachineStore");
var MaintenanceStore = require("../stores/MaintenanceStore");
var CompanyStore = require("../stores/CompanyStore");

var Machine = require("./Machine.react");

var Table = B.Table;
var Link = Router.Link;
var Well = B.Well;
var Input = B.Input;
var Button = B.Button;
var Glyphicon = B.Glyphicon;
var ButtonToolbar = B.ButtonToolbar;
var DropdownButton = B.DropdownButton;
var MenuItem = B.MenuItem;
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;

var DocumentTitle = require('react-document-title');

var RecordMaintenance = React.createClass({

  mixins: [listenToStoresMixin([MachineStore, MaintenanceStore, CompanyStore])]

  , computeStateFromStores: function () {
    var maintenanceId = this.props.params.maintenanceId;
    var maintenance = MaintenanceStore.get(maintenanceId);
    var company = CompanyStore.get(maintenance.companyId);
    var machines = MachineStore.getByCompanyId(maintenance.companyId);
    return ({
      "maintenance": maintenance
      , "company": company
      , "machines": machines
    });
  }

  , render: function() {

    var machines = _.reduce(this.state.machines, function(acc, machine, machineId) {
      var element = 
        <Row key={machineId}>
          <Machine selected={false} machine={machine} />
        </Row>;
      acc.push(element);
      return acc;
    }, []);

    return (
      <main>
        <h1>Zaznamenej servis</h1>
        <h2>{this.state.company["name"]}</h2>
        <Grid>
          {machines}
        </Grid>
      </main>
    );
  }

});

module.exports = RecordMaintenance;
