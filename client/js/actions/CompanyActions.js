/*
 * CompanyActions
 */

var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require('../constants/CompanyConstants');
var CompanyStore = require('../stores/CompanyStore');

var CompanyActions = {
	createCompany: function(company) {
		// server action in real
		var id = CompanyStore.nextId();
		var createdCompany = {};
		createdCompany[id] = company;
    AppDispatcher.handleServerAction({
      type: CompanyConstants.SERVER_CREATED_COMPANY
			, company: createdCompany
    });
	}
};

module.exports = CompanyActions;
