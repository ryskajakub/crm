/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var $ = require("jquery");

var ReactCalendar = require("react-calendar");
var Month = ReactCalendar.Month;
var Day = ReactCalendar.Day;
var formatDate = require("../../utils/formatDate");
var Moment = require("../../utils/Moment");

var B = require("react-bootstrap");
var Popover = B.Popover;
var Glyphicon = B.Glyphicon;

var CalendarPopover = React.createClass({

  propTypes: {
    calendar: React.PropTypes.object // TODO validate it's a moment, not just object #20
    , setValue: React.PropTypes.func
    , closeCalendar: React.PropTypes.func
    , initialAccuracy: React.PropTypes.string
  }

  , componentDidMount: function() {
    var t = this;
    var calendarPopover = $("#calendar-popover");
    $("html").click(function(event) {
      var isClickInsideCalendar = _.size(calendarPopover.has(event.target)) === 1;
      if (!isClickInsideCalendar) {
        $("html").off("click");
        t.props.closeCalendar();
      }
    });
  }

  , changeCalendar: function(value, field) {
    var t = this;
    return function() {
      t.setState({"calendar": t.state.calendar.add(value, field)});
    }
  }

  , getInitialState: function() {
    return {calendar: this.props.calendar || Moment()};
  }

  , handleCalendarClick: function(accuracy, moment, event) {
    event.stopPropagation();
    if (undefined !== this.props.setValue) {
      this.props.setValue(accuracy, moment);
    }
  }

  , render: function() {

    var calendar = this.state.calendar;

    return (
      <Popover placement="bottom" positionLeft={0} positionTop={40} id="calendar-popover">
        <div className="relative">
          <a className="prevMonthPager" onClick={this.changeCalendar(-1, "months")} href="javascript://">&lt;</a>
          <a className="nextMonthPager" onClick={this.changeCalendar(1, "months")} href="javascript://">&gt;</a>
          <a className="prevYearPager" onClick={this.changeCalendar(-1, "years")} href="javascript://">&lt;&lt;</a>
          <a className="nextYearPager" onClick={this.changeCalendar(1, "years")} href="javascript://">&gt;&gt;</a>
          <Month date={calendar} onClick={this.handleCalendarClick}>
            <Day onClick={this.handleCalendarClick} />
          </Month>
        </div>
      </Popover>
    );
  }

});

module.exports = CalendarPopover;
