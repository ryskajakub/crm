/**
 * @jsx React.DOM
 */
var React = require('react');

var B = require("react-bootstrap");
var ListGroup = B.ListGroup;
var ListGroupItem = B.ListGroupItem;
var Jumbotron = B.Jumbotron;
var Grid = B.Grid;
var Col = B.Col;
var Row = B.Row;
var Well = B.Well;
var Input = B.Input;
var Button = B.Button;
var Moment = require("../utils/Moment");
var _ = require("underscore");

var MonthLink = React.createClass({

  render: function() {
    var year = this.props.year;
    var month = this.props.month;

    var momentObject = Moment().set("year", year).set("month", month);
    var asString = momentObject.format("MMMM YYYY");

    if (this.state["checked"]) {
      return (
        <li><a href='javascript://' onClick={this.click} className="bg-primary">{asString}</a></li>
      );
    } else {
      return (
        <li><a href='javascript://' onClick={this.click}>{asString}</a></li>
      );
    }
  }

  , getInitialState: function() {
    return {"checked": false};
  }

  , click: function() {
    this.setState({"checked": !this.state.checked});
  }

});

var MaintenanceForm = React.createClass({

  render: function() {

    var now = Moment();
    var month = now.month();
    var year = now.year();

    function addYear(range, year) {
      return _.map(range, function(month) {
        return {
          "month": month
          , "year": year
        };
      });
    }

    var thisYear = _.range(month, 12);
    var nextYear = _.range(0, month);
    var months = _.union(addYear(thisYear, year), addYear(nextYear, year + 1));

    var monthsDOM = _.reduce(months, function(acc, date) {
      var domElem = <MonthLink year={date.year} month={date.month} />;
      acc.push(domElem);
      return acc;
    }, []);

    return(
      <Well>
        <h2>Naplánování servisu</h2>
        <strong>Datum</strong>
        <ul className="list-inline">
          {monthsDOM}
        </ul>
        <Input type="textarea" label="Poznámka" rows="5" />
        <Button bsStyle="primary">Naplánuj servis</Button>
      </Well>
    );
  }

});

module.exports = MaintenanceForm;
