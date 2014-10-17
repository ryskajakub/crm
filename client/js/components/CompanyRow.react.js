/**
 * @jsx React.DOM
 */

var React = require('react');
var CompanyActions = require('../actions/CompanyActions');

var Router = require('react-router');
var Link = Router.Link;

var CompanyRow = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
    var companyRow = this.props.companyRow;
    return(
      <tr>
        <td>
          <Link to='company-detail' params={{companyId: this.props.key}}>{companyRow["name"]}</Link>
        </td>
        <td>{companyRow["days"]}</td>
      </tr>
    );
  }

});

module.exports = CompanyRow;
