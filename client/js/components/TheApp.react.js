/**
 * @jsx React.DOM
 */
var React = require('react');
var Router = require('react-router');

var Route = Router.Route;
var Routes = Router.Routes;
var DefaultRoute = Router.DefaultRoute;

var Navigation = require("./Navigation.react");
var CompanyDetail = require("./CompanyDetail.react");
var CompanyNew = require("./CompanyNew.react");
var PlannedMaintenances = require("./PlannedMaintenances.react");
var Maintenance = require("./Maintenance.react");
var RecordMaintenance = require("./RecordMaintenance.react");
var MachineDetail = require("./MachineDetail.react");
var CompaniesList = require("./CompaniesList.react");

var TheApp = (
  <Routes location="history">
    <Route path="/" handler={Navigation}>
      <Route name="machine-detail" path="/company/:companyId/machine/:machineId" handler={MachineDetail} />
      <Route name="planned-maintenances" path="/planned-maintenances" handler={PlannedMaintenances} />
      <Route name="company-new" path="/company/new" handler={CompanyNew} />
      <Route name="company-detail" path="/company/:companyId" handler={CompanyDetail} />
      <Route name="company-edit" path="/company/:companyId/edit" handler={CompanyDetail} />
      <Route name="maintenance" path="/company/:companyId/maintenance/:maintenanceId" handler={Maintenance} />
      <Route name="record-maintenance" path="/record-maintenance/:maintenanceId" handler={RecordMaintenance} />
      <DefaultRoute name="table" handler={CompaniesList} />
    </Route>
  </Routes>
);

require("../actions/LocalInitialActions");

module.exports = TheApp
