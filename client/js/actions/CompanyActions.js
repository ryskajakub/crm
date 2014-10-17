/*
 * CompanyActions
 */

var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require('../constants/CompanyConstants');
var CompanyStore = require('../stores/CompanyStore');
var $ = require("jquery");

var CompanyActions = {
  createCompany: function(company) {
    var companyAsJSON = JSON.stringify(company)
    $.ajax({
      data: companyAsJSON
      , contentType: "application/json"
      , type: "POST"
      , url: "/api/companies/new"
      , success: function(data) {
        var dataAsJSObject = JSON.parse(data);
        var createdCompany = {};
        var serverGeneratedId = dataAsJSObject["id"];
        createdCompany[serverGeneratedId] = company;
        AppDispatcher.handleServerAction({
          type: CompanyConstants.SERVER_CREATED_COMPANY
          , company: createdCompany
        });
      }
      , error: function(error) {
        var status = error.status;
        if (409 === status) {
          AppDispatcher.handleServerAction({
            type: CompanyConstants.SERVER_CREATE_COMPANY_FAIL
            , companyNameError: "Takové jméno firmy již existuje"
          })
        }
      }
    });
  }
};

module.exports = CompanyActions;
