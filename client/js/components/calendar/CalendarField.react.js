/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var $ = require("jquery");
var Moment = require("../../utils/Moment");

var ReactCalendar = require("react-calendar");
var Month = ReactCalendar.Month;
var Day = ReactCalendar.Day;
var formatDate = require("../../utils/formatDate");

var CalendarPopover = require("./CalendarPopover.react");

var B = require("react-bootstrap");
var Popover = B.Popover;
var Glyphicon = B.Glyphicon;

var CalendarField = React.createClass({

  propTypes: {
    setValue: React.PropTypes.func.isRequired
    , initialDate: React.PropTypes.object // TODO validate it's a moment, not just object #20
    , allowMonth: React.PropTypes.bool
    , yearPrevNext: React.PropTypes.bool
    , initialAccuracy: React.PropTypes.string
  }

  , initialDateOrNow: function() {
    var moment = this.props.initialDate;
    return (
      (moment === undefined)
      ? Moment()
      : moment.clone()
    );
  }

  , getInitialState: function() {
    return ({
      "shown": false
      , "pickedDate": this.initialDateOrNow()
      , "accuracy": this.props.initialAccuracy
    });
  }

  , showCalendarPickerToggle: function() {
    this.setState({"shown": !this.state.shown});
  }

  , showCalendarPicker: function() {
    this.setState({"shown": true});
  }

  , closeCalendar: function() {
    this.setState({"shown": false});
  }

  , setValue: function(accuracy, date) {
    this.setState({
      shown: false
      , pickedDate: date
      , accuracy: accuracy
    });
    this.props.setValue(accuracy, date);
  }

  , render: function () {

    var calendarPopover =
      <CalendarPopover
        setValue={this.setValue}
        calendar={this.initialDateOrNow()}
        closeCalendar={this.closeCalendar}
      />

    var pickedDate = this.state.pickedDate;
    var accuracy = this.state.accuracy;
    var formattedDate =
      (undefined === accuracy || undefined === pickedDate)
      ? ""
      : formatDate({"accuracy": accuracy, "date": pickedDate});

    var field =
      <div className="input-group relative">
        <div className="input-group-addon">
          <Glyphicon glyph="calendar" onClick={this.showCalendarPickerToggle} />
        </div>
        <input type="text" onClick={this.showCalendarPicker} className="form-control"
          value={formattedDate} onChange={function() {}} />
        {this.state.shown ? calendarPopover : ""}
      </div>

    return field;
  }

});

module.exports = CalendarField;
