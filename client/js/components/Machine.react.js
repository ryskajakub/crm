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

var BgH2 = React.createClass({

  render:function() {
    return (
      this.props.background
      ? <h4 className="bg-primary">{this.props.children}</h4>
      : <h4>{this.props.children}</h4>
    );
  }

})

var Machine = React.createClass({

  render: function() {

    var type = this.props.type;
    var lastMaintenance = this.props.lastMaintenance;
    var imageSource = this.props.imageSource;

    var toggleServicePlanLabel =
      (this.state.inServicePlan)
      ? "Odebrat"
      : "Naplánovat" ;

    return(
      <Col md={2}>
        <Panel>
          <BgH2 background={this.state.inServicePlan}>{type}</BgH2>
          <dl>
            <dt>Další servis</dt>
            <dd>{lastMaintenance}</dd>
          </dl>
          <img src={imageSource} width="120" />
          <Button onClick={this.toggleInServicePlan} bsStyle="primary">
            <Glyphicon glyph="wrench" /> {toggleServicePlanLabel}
          </Button>
        </Panel>
      </Col>
    );
  }

  , toggleInServicePlan: function() {
    this.setState({"inServicePlan": !this.state.inServicePlan});
  }

  , getInitialState: function() {
    return {
      "inServicePlan": false
    };
  }

});

module.exports = Machine;
