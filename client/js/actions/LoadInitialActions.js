// initial fetch of all companies
var CrmApi = require("../server/CrmApi");
var fluxify = require("../utils/fluxifyJson");
var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require("../constants/CompanyConstants");

CrmApi.Company.list(function(data) {
  var companies = fluxify(data.items);
  console.log(companies);
  AppDispatcher.handleServerAction({
    type: CompanyConstants.SERVER_INITIAL_COMPANIES
    , companies: companies
  });
});
