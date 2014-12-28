#!/bin/bash
STR='{"upkeepClosed":false,"upkeepDate":{"precision":{"instance":"DayPrecision"},"year":1970,"month":5,"day":30,"instance":"YearMonthDay"},"upkeepMachines":[{"upkeepMachineNote":"ok","upkeepMachineMachineId":1,"instance":"UpkeepMachine"},{"upkeepMachineNote":"nook","upkeepMachineMachineId":2,"instance":"UpkeepMachine"}],"instance":"Upkeep"}'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/0/upkeeps/1/
