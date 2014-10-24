/**
 * @jsx React.DOM
 */
var React = require('react');

var ReactCalendar = require("react-calendar");
var Calendar = ReactCalendar.Calendar;
var Month = ReactCalendar.Month;
var Day = ReactCalendar.Day;

var EmployeeStore = require("../stores/EmployeeStore");
var MaintenanceStore = require("../stores/MaintenanceStore");
var listenToStoresMixin = require("../utils/listenToStoresMixin");
var MaintenanceActions = require("../actions/MaintenanceActions");
var CalendarPopover = require("./CalendarPopover.react");

var LinkedStateMixin = require("react/lib/LinkedStateMixin");

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

// key for the element in array, which is needed for array, null is not accepted
var NULL_KEY = -1;

var MaintenanceForm = React.createClass({

  mixins: [
    listenToStoresMixin([EmployeeStore, MaintenanceStore])
    , LinkedStateMixin
  ]

  , computeStateFromStores: function() {
    var employees = EmployeeStore.get();
    var maintenance = MaintenanceStore.get(this.props.maintenanceId) || {};

    return {
      "employeeId": (undefined === maintenance.employeeId ? NULL_KEY : maintenance.employeeId)
      , "employees": employees
      , "maintenanceDate": (undefined === maintenance.date ? {} : {
        "date": maintenance.date.date
        , "accuracy": maintenance.date.accuracy
      })
      , "note": maintenance.note
      , "calendar": (undefined === maintenance.date ? Moment() : maintenance.date.date)
    };
  }

  , selectEmployee: function(employeeId) {
    var employeeIdMaybeNull = (employeeId === NULL_KEY ? null : employeeId);
    this.setState({"employeeId": employeeId});
  }

  , closeCalendar: function() {
    this.setState({"calendarPickerShown": false});
  }

  , render: function() {

    var maintenanceDate = this.state["maintenanceDate"];

    var formattedDate =
      (undefined === maintenanceDate["date"])
      ? ""
      : (
        "Day" === maintenanceDate["accuracy"]
        ? maintenanceDate["date"].format("D.MMMM YYYY (dddd)")
        : maintenanceDate["date"].format("MMMM YYYY")
      );

    var popover = 
      <CalendarPopover handleCalendarClick={this.handleCalendarClick} 
        calendar={this.state.calendar.clone()}
        closeCalendar={this.closeCalendar} />;

    var noEmployeeSelected = (<MenuItem key={NULL_KEY} href="javascript://">---</MenuItem>);
    var employees = _.reduce(this.state.employees, function(acc, elem, key) {
      acc.push(<MenuItem key={key} href="javascript://">{elem.name}</MenuItem>);
      return acc;
    }, [noEmployeeSelected]);

    var selectedEmployee =
      (NULL_KEY === this.state.employeeId)
      ? {"name": "---"}
      : this.state.employees[this.state.employeeId];

    return(
      <form className="form-horizontal relative">
        <Row>
          <Col md={5} mdOffset={4}><h2>Servis</h2></Col>
        </Row>

        <Row className="form-group">
          <label className="control-label col-md-1 col-md-offset-3">
            Datum
          </label>
          <Col md={5}>
            <div className="input-group relative">
              <div className="input-group-addon">
                <Glyphicon glyph="calendar" onClick={this.showCalendarPickerToggle} />
              </div>
              <input type="text" onClick={this.showCalendarPicker} className="form-control"
                value={formattedDate} onChange={function() {}} />
              {this.state["calendarPickerShown"] ? popover : ""}
            </div>
          </Col>
        </Row>

        <Row className="form-group">
          <label className="control-label col-md-1 col-md-offset-3">
            Servisman
          </label>
          <div className="col-md-5">
            <DropdownButton className="scrollable-menu"
              onSelect={this.selectEmployee} title={selectedEmployee["name"]}>
              {employees}
            </DropdownButton>
          </div>
        </Row>

        <Input type="textarea" rows="6" label="Poznámka" valueLink={this.linkState("note")}
          labelClassName="col-md-1 col-md-offset-3" wrapperClassName="col-md-5" />

        <Row className="form-group">
          <Col md={5} mdOffset={4}>
            <Button bsStyle="primary" onClick={this.makeTheMaintenancePlan}>Naplánuj servis</Button>
          </Col>
        </Row>

      </form>
    );
  }

  , makeTheMaintenancePlan: function() {
    MaintenanceActions.recordMaintenancePlan();
  }

  , showCalendarPickerToggle: function() {
    this.setState({"calendarPickerShown": !this.state["calendarPickerShown"]});
  }

  , showCalendarPicker: function() {
    this.setState({"calendarPickerShown": true});
  }

  , handleCalendarClick: function(name, moment, event) {
    event.stopPropagation();
    this.setState({"maintenanceDate": {"date": moment, "accuracy": name}, "calendarPickerShown": false});
  }

});

module.exports = MaintenanceForm;
