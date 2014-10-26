/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Input = B.Input;

var _ = require("underscore");

var $ = require("jquery");
window.jQuery = $;
var ui = require("../../bower_components/jquery-ui/jquery-ui");

var MachineDetail = React.createClass({

  componentDidMount: function() {
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
              <Row>
                <Col mdOffset={2} md={10}>
                  <Input id="machine-type" type="text" />
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
