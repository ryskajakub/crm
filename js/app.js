moment.locale("cs");

App = Ember.Application.create();

var companies = [{
	id: 1
	, title: "Firma 1"
	, date: moment(new Date('12-27-2015'))
}, {
	id: 2
	, title: "Firma 2"
	, date: moment(new Date('1-28-2016'))
}]

App.Router.map(function() {
	this.resource('index', { path: '/' });
});

App.IndexRoute = Ember.Route.extend({
	model: function() {
		return companies;
	}
});

Ember.Handlebars.helper('format-date', function(moment) {
	return moment.format('LL');
});
