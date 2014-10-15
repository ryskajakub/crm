/**
 * @jsx React.DOM
 */

var React = require("react");
var CompanyActions = require("../actions/CompanyActions")

var Navigation = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
  	return (
			<div>
				<ul>
					<a href="#">Main Page</a>
				</ul>
				<this.props.activeRouteHandler />
			</div>
  	);
  }

});

module.exports = Navigation;
