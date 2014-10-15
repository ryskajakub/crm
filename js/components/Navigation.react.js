/**
 * @jsx React.DOM
 */

var React = require("react");
var CompanyActions = require("../actions/CompanyActions")

var Router = require('react-router');
var Link = Router.Link;

var Navigation = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
  	return (
			<div>
				<ul>
					<Link to='table'>Main Page</Link>
				</ul>
				<this.props.activeRouteHandler />
			</div>
  	);
  }

});

module.exports = Navigation;
