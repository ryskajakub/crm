#!/bin/bash

STR='{"machineType":{"upkeepPerMileage":10000,"machineTypeName":"BK 150","machineTypeManufacturer":"Remeza","instance":"MachineType"},"companyId":1,"instance":"Machine","machineOperationStartDate":{"precision":{"instance":"DayPrecision"},"year":1970,"month":5,"day":20,"instance":"YearMonthDay"},"initialMileage":999,"mileagePerYear":999}'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/machines/1/
