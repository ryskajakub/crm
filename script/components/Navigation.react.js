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
					<a href="javascript://" onClick={this.onClick}>Main Page</a>
				</ul>
				{this.props.children}
			</div>
  	);
  }

	, onClick: function() {
		CompanyActions.returnToMainPage();
	}

});

module.exports = Navigation;
