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

var togglableStateMixin = require("../utils/togglableStateMixin");

var PanelInfoable = React.createClass({
  render: function() {
    var selected = this.props.selected;
    var header = this.props.header;
    return (
      selected
      ? (<Panel header={header} bsStyle="info">{this.props.children}</Panel>)
      : (<Panel header={header}>{this.props.children}</Panel>)
    );
  }
});

var Machine = React.createClass({

  mixins: [togglableStateMixin()]

  , componentWillMount: function() {
    this.setState({"active": this.props.selected});
  }

  , render: function() {

    var machine = this.props.machine;

    var type = machine.type;
    var nextMaintenance = machine.nextMaintenance.format("MMMM YYYY");
    var image = machine.image;

    return(
      <Col md={2}>
        <PanelInfoable header={type} selected={this.state.active}>
          <dl>
            <dt>Další servis</dt>
            <dd>{nextMaintenance}</dd>
          </dl>
          <img src={image} width="120" />
          <a href="javascript://" onClick={this.toggle}>
            {
              (this.state.active)
              ? <span><Glyphicon glyph="minus" /> Odebrat</span>
              : <span><Glyphicon glyph="plus" /> Zařadit do servisu</span>
            }
          </a>
        </PanelInfoable>
      </Col>
    );
  }

});

module.exports = Machine;
