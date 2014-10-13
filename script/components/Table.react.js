/**
 * @jsx React.DOM
 */

var React = require("react");
var _ = require("underscore");

var getTable = function () {
	return {
		"rows": [1, 2, 4, 5]
	};
}

var Table = React.createClass({

  getInitialState: function() {
		return getTable();
  },

  /**
   * @return {object}
   */
  render: function() {

		var rows = this.state.rows;

		var rowsHtml = _.reduce(rows, function(acc, elem) {
			var elem = <tr><td>{elem}</td></tr>;
			acc.push(elem);
			return acc;
		}, []);

  	return (
			<table className="table table-stripped">
				<thead>
					<tr><th>Column 4</th></tr>
				</thead>
				<tbody>
					{rowsHtml}
				</tbody>
			</table>
  	);
  }

});

module.exports = Table;
