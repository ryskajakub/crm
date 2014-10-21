/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Col = B.Col;
var Well = B.Well;
var Panel = B.Panel;
var Button = B.Button;
var Glyphicon = B.Glyphicon;

var BigMachine = React.createClass({

  render: function() {

    var type = this.props.type;
    var lastMaintenance = this.props.maintenanceDate;
    var imageSource = this.props.image;

    return(
      <Col md={4}>
        <Panel>
          <h2><a href="javascript://">{type}</a></h2>
          <dl>
            <dt>Další servis</dt>
            <dd>{lastMaintenance}</dd>
          </dl>
          <img src={imageSource} width="240" />
        </Panel>
      </Col>
    );
  }
});

module.exports = BigMachine;
