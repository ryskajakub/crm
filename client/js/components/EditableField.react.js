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
    initialValue: PropTypes.string
    , editing: PropTypes.bool
    , setValue: PropTypes.func.isRequired
  }

  , getInitialState: function () {
    return {
      "value": (this.props.initialValue === undefined ? "" : this.props.initialValue)
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
    var t = this;
    return (
      editing
      ? (function() {
          var input = <Input type="text" value={value} onChange={t.onChange} />;
          var propsFromParent = _.omit(t.props, ["initialValue", "editing", "setValue"]);
          _.extend(input.props, propsFromParent);
          return input;
        })()
      : <span>{value}</span>
    );
  }

});

module.exports = EditableField;
