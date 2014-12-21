#!/bin/bash
STR='{"upkeepDate":{"precision":{"instance":"DayPrecision"},"year":1970,"month":5,"day":20,"instance":"YearMonthDay"},"upkeepMachines":[{"upkeepMachineNote":"ok","upkeepMachineMachineId":1,"instance":"UpkeepMachine"},{"upkeepMachineNote":"nook","upkeepMachineMachineId":2,"instance":"UpkeepMachine"}],"instance":"Upkeep"}'
curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/1/upkeeps/
