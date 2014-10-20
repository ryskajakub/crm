/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Col = B.Col;
var Well = B.Well;

var Machine = React.createClass({
  render: function() {

    var type = this.props.type;
    var lastMaintenance = this.props.lastMaintenance;
    var imageSource = this.props.imageSource;

    return(
      <Col md={6}>
        <Well>
          <ListGroup>
            <ListGroupItem><strong>Typ</strong> {type}</ListGroupItem>
            <ListGroupItem><strong>Poslední servis</strong> {lastMaintenance}</ListGroupItem>
          </ListGroup>
          <img src={imageSource} />
        </Well>
      </Col>
    );
  }
});

module.exports = Machine;
