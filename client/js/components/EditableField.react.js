/**
 * @jsx React.DOM
 */
var React = require('react');
var LinkedStateMixin = require('react/lib/LinkedStateMixin');

var PropTypes = React.PropTypes;

var B = require("react-bootstrap");
var Input = B.Input;

var EditableField = React.createClass({

  mixins: [LinkedStateMixin]

  , propTypes: {
    initialValue: PropTypes.string.isRequired
    , editing: PropTypes.bool
    , setValue: PropTypes.func.isRequired
  }

  , getInitialState: function () {
    return {
      "value": this.props.initialValue
    }
  }

  , onChange: function (event) {
    this.setState({"value": event.target.value})
    this.props.setValue(event);
  }

  /**
   * @return {object}
   */
  , render: function() {
    var editing = (undefined === this.props.editing ? false : this.props.editing);
    var value = this.state.value;
    return (
      editing
      ? <Input type="text" value={value} onChange={this.onChange} />
      : <span>{value}</span>
    );
  }

});

module.exports = EditableField;
