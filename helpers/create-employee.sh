#!/bin/bash
STR='{"instance":"Employee","name":"Aloisx12", "contact":"contact", "capabilities":"caps"}'
curl -X POST -H "Authorization: cGFzcw==" -H "Content-Type: application/json" -d "${STR}" http://crm/api/v1.0.0/employees/
