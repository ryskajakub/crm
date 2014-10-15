/**
 * @jsx React.DOM
 */

var React = require('react');
var CompanyActions = require('../actions/CompanyActions');

var CompanyRow = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
    var companyRow = this.props.companyRow;
		return(
			<tr>
				<td><a href="javascript://" onClick={this.onClick}>{companyRow["name"]}</a></td>
				<td>{companyRow["days"]}</td>
			</tr>
		);
	}

	, onClick: function() {
		var companyId = this.props.key;
		CompanyActions.showCompanyDetail(companyId);
	}

});

module.exports = CompanyRow;
