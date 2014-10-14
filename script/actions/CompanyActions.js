/*
 * CompanyActions
 */

var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require('../constants/CompanyConstants');

var CompanyActions = {

	showCompanyDetail: function(companyId) {
		var object = {
			actionType: CompanyConstants.COMPANY_SHOW_DETAIL
			, companyId: companyId
		};

		AppDispatcher.handleViewAction(object);
	}

	, returnToMainPage: function() {
		var object = {
			actionType: CompanyConstants.RETURN_TO_MAIN_PAGE
		}

		console.log(object);

		AppDispatcher.handleViewAction(object);
	}

};

module.exports = CompanyActions;
