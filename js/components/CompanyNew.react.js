/**
 * @jsx React.DOM
 */
var React = require('react');

var CompanyNew = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
		return(
			<form>
				<label htmlFor="company-name">Jméno</label> <input type="text" id="company-name" />
				<label htmlFor="company-days">Dny</label> <input type="text" id="company-days" />
				<input type="submit" />
			</form>
		);
	}

});

module.exports = CompanyNew;
