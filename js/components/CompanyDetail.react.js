/**
 * @jsx React.DOM
 */
var React = require('react');
var CompanyStore = require("../stores/CompanyStore");

var CompanyRow = React.createClass({

  /**
   * @return {object}
   */
  render: function() {

		return(
			<div>{this.state.name}</div>
		);
	}

	, getInitialState: function () {
		return this.getCompanyById(this.props.params.companyId);
	}

	, getCompanyById: function(id) {
		return CompanyStore.get(id);
	}

  , componentDidMount: function() {
    CompanyStore.addChangeListener(this.onChange);
  }

  , componentWillUnmount: function() {
    CompanyStore.removeChangeListener(this.onChange);
  }

	, onChange: function() {
		this.setState(this.getCompanyById(this.props.params.companyId));
	}

});

module.exports = CompanyRow;
