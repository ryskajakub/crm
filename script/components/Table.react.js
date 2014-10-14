/**
 * @jsx React.DOM
 */

var React = require("react");
var _ = require("underscore");
var CompanyRow = require("./CompanyRow.react");
var CompaniesStore = require("../stores/CompaniesStore");

var Table = React.createClass({

  getInitialState: function() {
		return CompaniesStore.getAll();
  },

  /**
   * @return {object}
   */
  render: function() {

		var rows = this.state;

		var rowsHtml = _.reduce(rows, function(acc, value, key) {
			var elem = 
				<CompanyRow companyRow={value} key={key} />
			acc.push(elem);
			return acc;
		}, []);

  	return (
			<table className="table table-stripped">
				<thead>
					<tr><th>Název</th><th>Detail</th></tr>
				</thead>
				<tbody>
					{rowsHtml}
				</tbody>
			</table>
  	);
  }

});

module.exports = Table;
