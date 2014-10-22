/**
 * @jsx React.DOM
 */
var React = require('react');

var ReactCalendar = require("react-calendar");
var Calendar = ReactCalendar.Calendar;
var Month = ReactCalendar.Month;
var Day = ReactCalendar.Day;

var EmployeeStore = require("../stores/EmployeeStore");
var listenToStoreMixin = require("../utils/listenToStoreMixin");

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
var Popover = B.Popover;
var Glyphicon = B.Glyphicon;
var MenuItem = B.MenuItem;
var DropdownButton = B.DropdownButton;
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

  mixins: [listenToStoreMixin(EmployeeStore, "employees", function(component, store) {
    return store.get();
  })]

  , selectEmployee: function(key) {
    this.setState({"employeeId": key});
  }

  , render: function() {

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

    var maintenanceDate = this.state["maintenanceDate"];

    var popover =
      <Popover placement="bottom" positionLeft={0} positionTop={125}>
        <Month date={Moment().subtract(100, "months")} onClick={this.handleCalendarClick}>
          <Day onClick={this.handleCalendarClick} />
        </Month>
      </Popover>

    var employees = _.reduce(this.state.employees, function(acc, elem, key) {
      acc.push(<MenuItem key={key} href="javascript://">{elem.name}</MenuItem>);
      return acc;
    }, []);

    var selectedEmployee = 
      (null === this.state.employeeId)
      ? this.state.employees["0"]
      : this.state.employees[this.state.employeeId];

    return(
      <Grid>
        <Row>
          <Col md={6} mdOffset={3}>
            <h2>Naplánování servisu</h2>
            <Input addonBefore={<Glyphicon glyph="calendar" onClick={this.showCalendarPickerToggle} />}
              type="text" label="Datum" value={maintenanceDate} onClick={this.showCalendarPicker} />
            {this.state["calendarPickerShown"] ? popover : ""}
            <DropdownButton className="scrollable-menu" onSelect={this.selectEmployee} title={selectedEmployee["name"]}>
              {employees}
            </DropdownButton>
            <Input type="textarea" label="Poznámka" rows="5" />
            <Button bsStyle="primary">Naplánuj servis</Button>
          </Col>
        </Row>
      </Grid>
    );
  }

  , showCalendarPickerToggle: function() {
    this.setState({"calendarPickerShown": !this.state["calendarPickerShown"]});
  }

  , showCalendarPicker: function() {
    this.setState({"calendarPickerShown": true});
  }

  , getInitialState: function() {
    return({
      "maintenanceDate": null
      , "calendarPickerShown": false
      , "employeeId": null
    });
  }

  , handleCalendarClick: function(name, moment, event) {
    event.stopPropagation();
    var formattedDate =
      ("Day" === name)
      ? moment.format("D.MMMM YYYY (dddd)")
      : moment.format("MMMM YYYY") ;

    this.setState({"maintenanceDate": formattedDate, "calendarPickerShown": false});

  }

});

module.exports = MaintenanceForm;
