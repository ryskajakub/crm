/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * @jsx React.DOM
 */

var React = require('react');
var Router = require('react-router');

var Route = Router.Route;
var Routes = Router.Routes;
var DefaultRoute = Router.DefaultRoute;

var Navigation = require("./components/Navigation.react");
var CompanyDetail = require("./components/CompanyDetail.react");
var Table = require("./components/Table.react");

var routes = (
	<Routes location="history">
		<Route path="/" handler={Navigation}>
			<Route name="company-detail" path="/company/:companyId" handler={CompanyDetail} />
			<DefaultRoute name="table" handler={Table} />
		</Route>
	</Routes>
);

React.renderComponent(
	routes, document.getElementById('crm-app')
);
