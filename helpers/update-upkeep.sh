#!/bin/bash
STR='[{"upkeepClosed":false,"upkeepDate":{"precision":{"instance":"DayPrecision"},"year":1970,"month":5,"day":30,"instance":"YearMonthDay"},"instance":"Upkeep"},[[{"upkeepMachineNote":"ffffok","instance":"UpkeepMachine","recordedMileage":11000},1]],[]]'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/0/upkeeps/1/
