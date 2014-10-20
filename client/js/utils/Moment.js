var Moment = require("moment");
var MomentLocale = require("moment/locale/cs");

Moment.defineLocale("cs", MomentLocale);
Moment.locale("cs");

module.exports = Moment;
