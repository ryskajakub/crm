#!/bin/bash

STR="{\"machineType\":{\"machineTypeName\":\"BK 150\",\"machineTypeManufacturer\":\"Remeza\",\"instance\":\"MachineType\"},\"companyId\":1,\"machineOperationStartDate\":\"1989\",\"instance\":\"Machine\"}"
curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/companies/1/machines/
