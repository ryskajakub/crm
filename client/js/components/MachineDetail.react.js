/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Input = B.Input;

var MachineDetail = React.createClass({

  render: function() {
    return(
      <Grid>
        <Row>
          <Col mdOffset={3} md={6}>
            <form className="form-horizontal relative">
              <Row>
                <Col mdOffset={2} md={10}>
                  <h1>Nové zařízení</h1>
                </Col>
              </Row>
              <Input type="text" label="Typ" onChange={this.changeTypeText}
                labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />
            </form>
          </Col>
        </Row>
      </Grid>
    );
  }

  , changeTypeText: function(event) {
    var value = event.target.value;
  }

});

module.exports = MachineDetail;
