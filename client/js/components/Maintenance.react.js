/**
 * @jsx React.DOM
 */
var React = require('react');

var Moment = require("../utils/Moment");
var listenToStoreMixin = require("../utils/listenToStoreMixin");

var Machine = require("./Machine.react");
var MaintenanceForm = require("./MaintenanceForm.react");

var MachineStore = require("../stores/MachineStore");

var B = require("react-bootstrap");
var Row = B.Row;

var _ = require("underscore");

var Maintenance = React.createClass({

  mixins: [listenToStoreMixin(MachineStore, "machines", function(component, store) {
    return store.getByCompanyId(component.props.params.companyId);
  })]

  , render: function() {

    var machines = _.reduce(this.state.machines, function (acc, value, key) {
      var element = <Machine key={key} machine={value} />
      acc.push(element);
      return acc;
    }, []);

    return (
      <main>
        <h1>Servis</h1>
        <h2>Continental</h2>
        <Row>{machines}</Row>
        <MaintenanceForm />
      </main>
    );
  }

});

module.exports = Maintenance;
