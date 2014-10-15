/**
 * @jsx React.DOM
 */

var React = require('react');
var CompanyConstants = require('../constants/CompanyConstants');
var Table = require("./Table.react");
var CompanyDetail = require("./CompanyDetail.react");
var Navigation = require("./Navigation.react");

var App = React.createClass({

	getInitialState: function() {
		return CurrentPageStore.get();
	}

  /**
   * @return {object}
   */
  , render: function() {
		return (
			<Routes location="history" >
				<Route path="/" handler={Navigation} >
					<Route name="company-detail" path="/company/:companyId" handler={CompanyDetail} />
					<DefaultRoute handler={Table} />
				</Route>
			</Routes>
		);
	}

});

module.exports = App;
