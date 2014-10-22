/**
 * @jsx React.DOM
 */
var React = require('react');

var _ = require("underscore");
var Moment = require("../utils/Moment");

var Router = require('react-router');
var B = require("react-bootstrap");

var listenToStoreMixin = require("../utils/listenToStoreMixin");
var MaintenanceStore = require("../stores/MaintenanceStore");

var Table = B.Table;
var Link = Router.Link;

var DocumentTitle = require('react-document-title');

var PlannedMaintenances = React.createClass({

  mixins: [listenToStoreMixin(MaintenanceStore, "maintenances", function(component, store) {
    return store.get();
  })]

  , render: function() {

    var planned = [];

    var rows = _.reduce(planned, function(acc, elem) {
      var row = 
        <tr>
          <td>
            <a href="javascript://">{elem[0]}</a>
          </td>
          <td>
            {elem[1]}
          </td>
          <td>
            <a href="javascript://">Upravit</a>
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
