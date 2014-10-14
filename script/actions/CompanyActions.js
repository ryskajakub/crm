/*
 * CompanyActions
 */

var AppDispatcher = require('../dispatcher/AppDispatcher');
var CompanyConstants = require('../constants/CompanyConstants');

var CompanyActions = {

	showCompanyDetail: function(companyId) {
		AppDispatcher.handleViewAction({
			actionType: CompanyConstants.COMPANY_SHOW_DETAIL
			, companyId: companyId
		});
	}

};

module.exports = CompanyActions;
