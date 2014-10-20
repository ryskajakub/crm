/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Col = B.Col;
var Well = B.Well;
var Button = B.Button;
var Glyphicon = B.Glyphicon;

var BgH2 = React.createClass({

  render:function() {
    return (
      this.props.background
      ? <h2 className="bg-primary">{this.props.children}</h2>
      : <h2>{this.props.children}</h2>
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
      ? "Odebrat ze servisního plánu"
      : "Přidat do servisního plánu" ;

    return(
      <Col md={6}>
        <Well>
          <BgH2 background={this.state.inServicePlan}>{type}</BgH2>
          <ListGroup>
            <ListGroupItem><strong>Poslední servis</strong> {lastMaintenance}</ListGroupItem>
          </ListGroup>
          <img src={imageSource} />
          <Button onClick={this.toggleInServicePlan} bsStyle="primary">
            <Glyphicon glyph="wrench" /> {toggleServicePlanLabel}
          </Button>
        </Well>
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
