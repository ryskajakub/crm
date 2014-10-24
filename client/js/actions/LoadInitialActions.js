
// initial fetch of all companies
$.ajax({
  url: "/api/companies"
  , success: function(data) {
    var companies = JSON.parse(data);
    AppDispatcher.handleServerAction({
      type: CompanyConstants.SERVER_INITIAL_COMPANIES
      , companies: companies
    })
  }
})
