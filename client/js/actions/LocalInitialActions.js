var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require("../constants/CompanyConstants");

var id = 0;

function createCompany(name) {
  var company = {
    id: id
    , name: name
    , active: true
  };
  id += 1;
  return company;
}

var companies = [
  createCompany("Continental")
  , createCompany("České dráhy")
  , createCompany("FOMA Bohemia")
  , createCompany("Kand")
  , createCompany("Metrostav")
  , createCompany("Neumann")
  , createCompany("PREX")
  , createCompany("Stachema Kolín")
  , createCompany("Valsabbia")
];

AppDispatcher.handleServerAction({
  type: CompanyConstants.SERVER_INITIAL_COMPANIES
  , companies: companies
});
