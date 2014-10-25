/**
 * @jsx React.DOM
 */
var React = require('react');

var PropTypes = React.PropTypes;

var B = require("react-bootstrap");
var Input = B.Input;

var EditableField = React.createClass({

  propTypes: {
    initialValue: PropTypes.string.isRequired
    , editing: PropTypes.bool
  }

  , getInitialState: function() {
    return {
      "value": this.props.initialValue
    };
  }

  /**
   * @return {object}
   */
  , render: function() {
    var value = this.state.value;
    var editing = (undefined === this.props.editing ? false : this.props.editing);
    return (
      editing
      ? <Input type="text" value={value} />
      : value
    );
  }

});

module.exports = EditableField;
