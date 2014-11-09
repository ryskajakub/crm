var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require("../constants/CompanyConstants");
var MachineConstants = require("../constants/MachineConstants");
var Moment = require("../utils/Moment");

// companies
var id = 0;

var companies = {};

function createCompany(name, nextService) {
  var company = {
    name: name
    , plant: "I"
    , active: true
    , contact: "p. Jelínek"
    , phone: "721 650 194"
    , address: "Brandýs nad labem"
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

// machines
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
createMachine(1, "/images/atlas2.jpg", "Z 55-900", Moment().year(2015).month(5));
createMachine(1, "/images/atlas1.jpg", "XAHS 836", Moment().year(2015).month(7));

AppDispatcher.handleServerAction({
  type: MachineConstants.SERVER_INITIAL_MACHINES
  , machines: machines
});

// employees
var employeeId = 0;
var employees = {};

function createEmployee(name) {
  var employee = {
    "name": name
  };
  employees[employeeId] = employee;
  employeeId += 1;
}

createEmployee("Kutička");
createEmployee("Mandlík");

AppDispatcher.handleServerAction({
  type: CompanyConstants.SERVER_INITIAL_EMPLOYEES
  , employees: employees
});

// maintenances
var maintenanceId = 0;
var maintenances = {};

function createMaintenance(date, accuracy, companyId, machines, note, serviceman) {
  var maintenance = {
    "date" : {
      "date": date
      , "accuracy": accuracy
    }
    , "companyId": companyId
    , "machines": machines
    , "note": note
    , "employeeId": serviceman
  };
  maintenances[maintenanceId] = maintenance;
  maintenanceId += 1;
}

createMaintenance(Moment().add(5, "months"), "Day", 0, [{"machineId": 0}, {"machineId": 1}], "Bude třeba vyměnit filtr", 0);
createMaintenance(Moment().add(10, "months"), "Month", 0, [{"machineId": 0}], "Bude třeba vyměnit řemen", 1);
createMaintenance(Moment().add(11, "months"), "Day", 1, [{"machineId": 2}], "Bude třeba vyhodit atlas copco compresory", 1);

AppDispatcher.handleServerAction({
  type: MachineConstants.SERVER_INITIAL_MAINTENANCES
  , maintenances: maintenances
});
