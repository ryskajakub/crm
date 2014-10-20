/**
 * @jsx React.DOM
 */

var React = require('react');
var Moment = require("../utils/Moment");

var CompanyActions = require('../actions/CompanyActions');

var Router = require('react-router');
var Link = Router.Link;

var CompanyRow = React.createClass({

  /**
   * @return {object}
   */
  render: function() {
    var companyRow = this.props.companyRow;

    var maintenanceAfter = "";
    if(companyRow["nextService"]) {
      var nextService = Moment(companyRow["nextService"]);
      var now = Moment()
      var difference = Moment.duration(now.diff(nextService));
      maintenanceAfter = difference.humanize();
    } else {
      maintenanceAfter = Moment.duration(1, "years").humanize();
    }

    return(
      <tr>
        <td>
          <Link to='company-detail' params={{companyId: this.props.key}}>{companyRow["name"]}</Link>
        </td>
        <td>{maintenanceAfter}</td>
      </tr>
    );
  }

});

module.exports = CompanyRow;
