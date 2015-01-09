#!/bin/bash
STR='[{"instance":"MachineType","machineTypeName":"ABC 100","machineTypeManufacturer":"REMEZA","upkeepPerMileage":500},[{"instance":"UpkeepSequence","displayOrdering":1,"label_":"4000mth","repetition":500},{"instance":"UpkeepSequence","displayOrdering":2,"label_":"General","repetition":10000}]]'
curl -X PUT -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/machine-types/by-id/2/
