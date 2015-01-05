#!/bin/bash
STR='{"instance":"MachineType","machineTypeName":"ABC 100","machineTypeManufacturer":"REMEZA","upkeepPerMileage":500}'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/machine-types/by-id/2/
