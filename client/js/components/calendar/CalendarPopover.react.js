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

  , subtractMonth: function(event) {
    this.setState({"calendar": this.state.calendar.subtract(1, "months")});
  }

  , addMonth: function(event) {
    this.setState({"calendar": this.state.calendar.add(1, "months")});
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
          <a className="leftPager" onClick={this.subtractMonth} href="javascript://">&lt;&lt;</a>
          <a className="rightPager" onClick={this.addMonth} href="javascript://">&gt;&gt;</a>
          <Month date={calendar} onClick={this.handleCalendarClick}>
            <Day onClick={this.handleCalendarClick} />
          </Month>
        </div>
      </Popover>
    );
  }

});

module.exports = CalendarPopover;
