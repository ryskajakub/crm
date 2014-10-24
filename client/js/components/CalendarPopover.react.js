/**
 * @jsx React.DOM
 */
var React = require('react');
var _ = require("underscore");
var $ = require("jquery");

var ReactCalendar = require("react-calendar");
var Month = ReactCalendar.Month;
var Day = ReactCalendar.Day;

var B = require("react-bootstrap");
var Popover = B.Popover;

var CalendarPopover = React.createClass({

  componentDidMount: function() {
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

  , componentWillMount: function() {
    this.setState({"calendar": this.props.calendar});
  }

  , subtractMonth: function(event) {
    this.setState({"calendar": this.state.calendar.subtract(1, "months")});
  }

  , addMonth: function(event) {
    this.setState({"calendar": this.state.calendar.add(1, "months")});
  }

  , render: function() {

    var calendar = this.state.calendar;
    var handleCalendarClick = this.props.handleCalendarClick;

    var popover =
      <Popover placement="bottom" positionLeft={0} positionTop={40} id="calendar-popover">
        <div className="relative">
          <a className="leftPager" onClick={this.subtractMonth} href="javascript://">&lt;&lt;</a>
          <a className="rightPager" onClick={this.addMonth} href="javascript://">&gt;&gt;</a>
          <Month date={calendar} onClick={handleCalendarClick}>
            <Day onClick={handleCalendarClick} />
          </Month>
        </div>
      </Popover>
    return popover;
  }

});

module.exports = CalendarPopover;
