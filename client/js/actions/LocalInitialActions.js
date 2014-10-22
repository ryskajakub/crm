var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require("../constants/CompanyConstants");
var MachineConstants = require("../constants/MachineConstants");
var Moment = require("../utils/Moment");

var id = 0;

var companies = {};

function createCompany(name, nextService) {
  var company = {
    name: name
    , active: true
  };
  companies[id] = company;
  if (undefined !== nextService) {
    companies[id].nextService = nextService;
  }
  id += 1;
}

createCompany("Continental", "2.8.2015");
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

function createMachine(companyId, image, type, nextMaintenance) {
  var machine = {
    "companyId": companyId
    , "image": image
    , "type": type
    , "nextMaintenance": nextMaintenance
  }
  machines[machineId] = machine;
  machineId += 1;
}

createMachine(0, "/images/remeza-bk15e.jpg", "BK 15", Moment().year(2015).month(2));
createMachine(0, "/images/pistovy-kompresor-remeza-360-l-min-400-v.jpg", "C-50.AB360", Moment().year(2015).month(3));

AppDispatcher.handleServerAction({
  type: MachineConstants.SERVER_INITIAL_MACHINES
  , machines: machines
});
