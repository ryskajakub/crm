/**
 * @jsx React.DOM
 */
var React = require('react');
var CompanyDetailStore = require("../stores/CompanyDetailStore");

var CompanyRow = React.createClass({

  getInitialState: function() {
		return CompanyDetailStore.get();
  },

  /**
   * @return {object}
   */
  render: function() {

		return(
			<div>{this.state.company}</div>
		);
	}

  , componentDidMount: function() {
    CompanyDetailStore.addChangeListener(this.onChange);
  }

  , componentWillUnmount: function() {
    CompanyDetailStore.removeChangeListener(this.onChange);
  }

	, onChange: function() {
		this.setState(
			CompanyDetailStore.get()
		);
	}

});

module.exports = CompanyRow;
