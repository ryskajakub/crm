#!/bin/bash
STR='{"instance":"Employee","name":"Alois"}'
curl -X POST -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/employees/
