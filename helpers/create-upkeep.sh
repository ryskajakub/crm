#!/bin/bash
STR='[{"workHours":"555","workDescription":"Nothing done really","recommendation":"no recommendation","upkeepClosed":false,"upkeepDate":{"precision":{"instance":"DayPrecision"},"year":1970,"month":5,"day":20,"instance":"YearMonthDay"},"instance":"Upkeep"},[[{"upkeepMachineNote":"ok","instance":"UpkeepMachine","recordedMileage":11000},1],[{"upkeepMachineNote":"nook","instance":"UpkeepMachine","recordedMileage":1234},2]],{"instance":"MyJust","slot1":2}]'
curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/1/upkeeps/
