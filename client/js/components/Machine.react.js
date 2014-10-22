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

    var machine = this.props.machine;

    var type = machine.type;
    var nextMaintenance = machine.nextMaintenance.format("MMMM YYYY");
    var image = machine.image;

    return(
      <Col md={2}>
        <PanelInfoable header={type}>
          <dl>
            <dt>Další servis</dt>
            <dd>{nextMaintenance}</dd>
          </dl>
          <img src={image} width="120" />
          <a href="javascript://">
            <Glyphicon glyph="plus" /> Zařadit do servisu
          </a>
        </PanelInfoable>
      </Col>
    );
  }

});

module.exports = Machine;
