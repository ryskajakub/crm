/**
 * @jsx React.DOM
 */
var React = require('react');
var Router = require('react-router');

var Route = Router.Route;
var Routes = Router.Routes;
var DefaultRoute = Router.DefaultRoute;

var Navigation = require("./Navigation.react");
var CompanyDetail = require("./CompanyDetail.react");
var Table = require("./Table.react");

var TheApp = (
	<Routes location="history">
		<Route path="/" handler={Navigation}>
			<Route name="company-detail" path="/company/:companyId" handler={CompanyDetail} />
			<DefaultRoute name="table" handler={Table} />
		</Route>
	</Routes>
);

module.exports = TheApp
