/**
 * @jsx React.DOM
 */

var React = require('react');
var CompanyConstants = require('../constants/CompanyConstants');
var Table = require("./Table.react");
var CompanyDetail = require("./CompanyDetail.react");
var Navigation = require("./Navigation.react");
var CurrentPageStore = require("../stores/CurrentPageStore");
var _ = require("underscore")

var AppMainPage = React.createClass({

	getInitialState: function() {
		return CurrentPageStore.get();
	}

  /**
   * @return {object}
   */
  , render: function() {
		var page = this.state.page;

		if (page == CompanyConstants.PAGE_MAIN) {
			return(<Navigation><Table /></Navigation>);
		} else if (page == CompanyConstants.PAGE_COMPANY_DETAIL) {
			return(<Navigation><CompanyDetail /></Navigation>);
		}
	}

  , componentDidMount: function() {
    CurrentPageStore.addChangeListener(this.onPageChange);
  }

  , componentWillUnmount: function() {
    CurrentPageStore.removeChangeListener(this.onPageChange);
  }

	, onPageChange: function() {
		this.setState(
			CurrentPageStore.get()
		);
	}

});

module.exports = AppMainPage;
