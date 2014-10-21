/**
 * @jsx React.DOM
 */
var React = require('react');

var Moment = require("../utils/Moment");

var Machine = require("./Machine.react");
var MaintenanceForm = require("./MaintenanceForm.react");

var B = require("react-bootstrap");
var Row = B.Row;

var Maintenance = React.createClass({

  render: function() {

    return (
      <main>
        <h1>Servis</h1>
        <h2>Continental</h2>
        <Row>
          <Machine image="/images/remeza-bk15e.jpg" type="BK 15" maintenanceDate="4 měsíce" />
          <Machine image="/images/pistovy-kompresor-remeza-360-l-min-400-v.jpg" type="C-50.AB360" maintenanceDate="5 měsíců" />
        </Row>
        <MaintenanceForm />
      </main>
    );
  }

});

module.exports = Maintenance;
