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

var PanelInfoable = React.createClass({
  render: function() {
    var selected = false;
    var header = this.props.header;
    return (
      selected
      ? (<Panel header={header} bsStyle="info">{this.props.children}</Panel>)
      : (<Panel header={header}>{this.props.children}</Panel>)
    );
  }
});

var Machine = React.createClass({

  render: function() {

    var type = this.props.type;
    var lastMaintenance = this.props.maintenanceDate;
    var imageSource = this.props.image;

    return(
      <Col md={2}>
        <PanelInfoable header={type}>
          <dl>
            <dt>Další servis</dt>
            <dd>{lastMaintenance}</dd>
          </dl>
          <img src={imageSource} width="120" />
          <a href="javascript://">
            <Glyphicon glyph="plus" /> Zařadit do servisu
          </a>
        </PanelInfoable>
      </Col>
    );
  }

});

module.exports = Machine;
