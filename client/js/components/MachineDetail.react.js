/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Input = B.Input;
var Button = B.Button;

var LinkedStateMixin = require("react/lib/LinkedStateMixin");
var _ = require("underscore");

var $ = require("jquery");
window.jQuery = $;
var ui = require("../../bower_components/jquery-ui/jquery-ui");

var MachineDetail = React.createClass({

  mixins: [LinkedStateMixin]

  , getInitialState: function() {
    return {
      "type": ""
    };
  }

  , componentDidMount: function() {
    $("#machine-type").autocomplete({
      source: [
        "ActionScript",
        "AppleScript",
        "Asp",
        "BASIC",
        "C",
        "C++",
        "Clojure",
        "COBOL",
        "ColdFusion",
        "Erlang",
        "Fortran",
        "Groovy",
        "Haskell",
        "Java",
        "JavaScript",
        "Lisp",
        "Perl",
        "PHP",
        "Python",
        "Ruby",
        "Scala",
        "Scheme"
      ]
    });
  }

  , render: function() {
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
              <Input id="machine-type" type="text" label="Typ" valueLink={this.linkState("type")}
                labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />
              <Input id="machine-manufacturer" type="text" label="Výrobce" valueLink={this.linkState("manufacturer")}
                labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />
              <Input type="text" label="Výr. čislo" valueLink={this.linkState("serialNumber")}
                labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />
              <Input type="text" label="Označení" valueLink={this.linkState("mark")}
                help="Označení stroje v rámci firmy aby se poznaly 2 stejného typu"
                labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />
              <Row className="form-group">
                <Col md={10} mdOffset={2}>
                  Řada servisů - TODO
                </Col>
              </Row>
              <Row className="form-group">
                <Col md={2} className="control-label">
                  <label>Úv. stav</label>
                </Col>
                <Col md={1} className="control-label">
                  <label>mth:</label>
                </Col>
                <Col md={2}>
                  <input type="text" className="form-control" />
                </Col>
                <Col md={1} className="control-label" mdOffset={1}>
                  <label>dne:</label>
                </Col>
                <Col md={5}>
                  <input type="text" className="form-control" />
                </Col>
              </Row>
              <Row className="form-group">
                <Col mdOffset={2} md={10}>
                  <Button bsStyle="primary">Zadej zařízení do systému</Button>
                </Col>
              </Row>
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
