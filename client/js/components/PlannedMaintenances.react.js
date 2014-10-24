/**
 * @jsx React.DOM
 */
var React = require('react');

var _ = require("underscore");
var Moment = require("../utils/Moment");

var Router = require('react-router');
var B = require("react-bootstrap");

var listenToStoresMixin = require("../utils/listenToStoresMixin");
var MaintenanceStore = require("../stores/MaintenanceStore");
var CompanyStore = require("../stores/CompanyStore");

var Table = B.Table;
var Link = Router.Link;

var DocumentTitle = require('react-document-title');

var PlannedMaintenances = React.createClass({

  mixins: [listenToStoresMixin([CompanyStore, MaintenanceStore])]

  , computeStateFromStores: function() {
    var maintenances = MaintenanceStore.get();
    var maintenancesWithCompanyName = _.map(maintenances, function(maintenance) {
      var company = CompanyStore.get(maintenance.companyId);
      maintenance["companyName"] = company["name"];
      return maintenance;
    });
    return {"maintenances": maintenancesWithCompanyName};
  }

  , render: function() {

    var rows = _.reduce(this.state["maintenances"], function(acc, elem, key) {
      var row = 
        <tr>
          <td>
            <Link to='company-detail' params={{companyId: elem["companyId"]}}>{elem["companyName"]}</Link>
          </td>
          <td>
            {elem["date"]["date"].format("MMMM YYYY")}
          </td>
          <td>
            <Link to="maintenance" params={{companyId: elem["companyId"], maintenanceId: key}}>Přeplánovat</Link>
          </td>
          <td>
            <Link to="record-maintenance" params={{companyId: "abc", maintenanceId: "abc"}}>Uzavřít</Link>
          </td>
        </tr>;
      acc.push(row);
      return acc;
    }, []);

    return (
      <DocumentTitle title={"CRM - Naplánované servisy"}>
        <main>
          <Table striped bordered>
            <thead>
              <tr><th>Název firmy</th><th>Servis naplánován</th><th>Upravit plán servisu</th><th>Uzavřít servis</th></tr>
            </thead>
            <tbody>
              {rows}
            </tbody>
          </Table>
        </main>
      </DocumentTitle>
    );
  }

});

module.exports = PlannedMaintenances
