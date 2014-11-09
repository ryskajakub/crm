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
var EditableField = require("./EditableField.react");

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
var Panel = B.Panel;


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
    var t = this;
    var editing = (this.props.name === "record-maintenance");
    var machines = _.reduce(this.state.machines, function(acc, machine, machineId) {

      var machineServiceData = _.find(t.state.maintenance.machines, function(obj) {
        return obj.machineId === parseInt(machineId);
      });
      if (undefined !== machineServiceData) {
        var element =
          <Row key={machineId}>
            <Col md={2}>
              <Panel header={machine.type}>
                <img src={machine.image} width="120" />
              </Panel>
            </Col>
            <Col md={2}>
              <EditableField label="Počet motohodin" setValue={function() {}}
                editing={editing} initialValue={machineServiceData.mth} />
            </Col>
            <Col md={4}>
              <EditableField label="Poznámka" type="textarea" rows={5} setValue={function() {}}
                editing={editing} initialValue={machineServiceData.note} />
            </Col>
          </Row>;
        acc.push(element);
        return acc;
      } else {
        return acc;
      }
    }, []);

    return (
      <main>
        <h1>Zaznamenej servis</h1>
        <h2>{this.state.company["name"]}</h2>
        <Grid>
          {machines}
          <Row>
            <form className="form-horizontal">
              <Input type="textarea" label="Zápis" labelClassName="col-md-2" wrapperClassName="col-md-10" />
              <Col md={10} mdOffset={2}>
                <Button bsStyle="primary">Uzavři servis</Button>
              </Col>
            </form>
          </Row>
        </Grid>
      </main>
    );
  }

});

module.exports = RecordMaintenance;
