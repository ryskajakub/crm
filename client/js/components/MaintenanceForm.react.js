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
var CalendarField = require("./calendar/CalendarField.react");

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

  , render: function() {

    var maintenanceDate = this.state["maintenanceDate"];

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
      <Col md={6} mdOffset={3}>
        <form className="form-horizontal relative">
          <Row>
            <Col md={10} mdOffset={2}><h2>Servis</h2></Col>
          </Row>

          <Row className="form-group">
            <label className="control-label col-md-2">
              Datum
            </label>
            <Col md={10}>
              <CalendarField 
                setValue={this.setMaintenanceDate} 
                initialDate={this.state.calendar.clone()} 
                allowMonth={true}
              />
            </Col>
          </Row>

          <Row className="form-group">
            <label className="control-label col-md-2">
              Servisman
            </label>
            <div className="col-md-10 scrollable-menu">
              <DropdownButton onSelect={this.selectEmployee} title={selectedEmployee["name"]}>
                {employees}
              </DropdownButton>
            </div>
          </Row>

          <Input type="textarea" rows="6" label="Poznámka" valueLink={this.linkState("note")}
            labelClassName="col-md-2" wrapperClassName="col-md-10" groupClassName="row" />

          <Row className="form-group">
            <Col md={10} mdOffset={2}>
              <Button bsStyle="primary" onClick={this.makeTheMaintenancePlan}>Naplánuj servis</Button>
            </Col>
          </Row>

        </form>
      </Col>
    );
  }

  , makeTheMaintenancePlan: function() {
    MaintenanceActions.recordMaintenancePlan();
  }

  , setMaintenanceDate: function(accuracy, moment, event) {
    this.setState({"maintenanceDate": {"date": moment, "accuracy": accuracy}});
  }

});

module.exports = MaintenanceForm;
