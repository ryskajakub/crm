var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require("../constants/CompanyConstants");
var MachineConstants = require("../constants/MachineConstants");

var id = 0;

var companies = {};

function createCompany(name) {
  var company = {
    name: name
    , active: true
  };
  companies[id] = company;
  id += 1;
}

createCompany("Continental");
createCompany("České dráhy");
createCompany("FOMA Bohemia");
createCompany("Kand");
createCompany("Metrostav");
createCompany("Neumann");
createCompany("PREX");
createCompany("Stachema Kolín");
createCompany("Valsabbia");

AppDispatcher.handleServerAction({
  type: CompanyConstants.SERVER_INITIAL_COMPANIES
  , companies: companies
});

var machineId = 0;
var machines = {};

function createMachine(companyId, image, type, lastMaintenance) {
  var machine = {
    "companyId": companyId
    , "image": image
    , "type": type
    , "lastMaintenance": lastMaintenance
  }
  machines[machineId] = machine;
  machineId += 1;
}

createMachine(0, "/images/remeza-bk15e.jpg", "BK 15", "2.8.2014");
createMachine(0, "/images/pistovy-kompresor-remeza-360-l-min-400-v.jpg", "C-50.AB360", "2.8.2013");

AppDispatcher.handleServerAction({
  type: MachineConstants.SERVER_INITIAL_MACHINES
  , machines: machines
});
